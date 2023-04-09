{-# LANGUAGE DeriveGeneric, DeriveFunctor, LambdaCase, NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
module BlockChain where

import Data.List (find, foldl1', foldl')
import Hashing ( shash256, TargetHash, HashOf )
import BlockType
    ( blockBlockHeight,
      blockPreviousHash,
      Block(Block, transactions, blockHeader, coinbaseTransaction),
      BlockHeader(previousHash),
      BlockReference, Output (Output) )
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import Data.Functor ( (<&>) )
import BlockValidation (UTXOPool (UTXOPool), UTXO(..), txGetNewUTXOs, validateBlock, validateNonce, coinbaseGetNewUTXOs)
import qualified Data.Map as Map
import BlockCreation (blockRef)
import Data.Maybe (fromJust, isJust, maybe)
import Control.Arrow ((&&&))
import qualified Data.Set as Set
import qualified Data.Tree as DTree -- Didn't realize it's there
import Data.Set (Set)

-- Returns a leaf furthest from root, "arbitrary" one if there's a draw
-- Nothing if LivelyBlocks empty.
getLastBlock :: LivelyBlocks -> Maybe Block
getLastBlock Lively {forest=[]} = Nothing
getLastBlock Lively {forest} = Just . snd $ foldl1' comp $ map (go 0) forest
    where
        comp a@(n, _) (m, _) | n >= m = a
        comp _ b = b
        go n (Tree b []) = (n, b)
        go n (Tree _ ts)   =  foldl1' comp . map (go (n+1)) $ ts

-- TODO: Now pruning is unnecessarily run on the whole tree at every insert. This can be improved. 

-- Forks significantly shorter (more than maxdiff shorter) than the longest one can be deleted.
prune :: Integer -> [Tree a] -> [Tree a]
prune maxdiff trees = map pruneA $ filter (\(Tree (d, _) _) -> d >= maxHeight - maxdiff) marked

    where
        -- Mark every node with max depth among leafs in the subtree of a node  
        markWithHeight :: Integer -> Tree a -> Tree (Integer, a)
        markWithHeight d (Tree a []) = Tree (d, a) []
        markWithHeight d (Tree a ts) =
            let tt@( Tree (d0, _) _ : ts') = map (markWithHeight (d+1)) ts in
            let maxd = foldl' (\md (Tree (dd, _) _) -> max md dd) d0 ts' in
            Tree (maxd, a) tt

        marked = map (markWithHeight 0) trees

        maxHeight = foldl1' max $ map (\case Tree (h, _) _ -> h) marked

        -- Discard tree's that don't have any leafs at depth at least equal to (maxHeight - maxdiff).
        pruneA :: Tree (Integer, a) -> Tree a
        pruneA (Tree (_, a) ts) = Tree a (map pruneA . filter (\(Tree (d, _) _) -> d >= maxHeight - maxdiff) $ ts)


newtype ForkMaxDiff = ForkMaxDiff Integer deriving (Generic)

instance ToJSON ForkMaxDiff
instance FromJSON ForkMaxDiff


height :: Tree a -> Integer
height (Tree _ []) = 0
height (Tree _ ts) = 1 + foldl1' max (map height ts)

-- If the tree starting from root looks like a linked list, then split the tree into the list part and the rest.
-- The first element in the list is parent of the root of the tree.
-- Height of a tree after this operation is at least maxdiff (TODO: think of a name instead of maxdiff).  
fixBlocks :: Integer -> [Tree a] -> ([a], [Tree a])
fixBlocks maxdiff trees = go (max (h - maxdiff) 0) [] trees

    where

        h = foldl1' max $ map height trees

        -- recurse on the tree collecting nodes, but collecting no more than given number
        go 0 ls trees = (ls, trees)
        go n ls [Tree a ts] = go (n-1) (a : ls) ts
        go n ls trees = (ls, trees)


data BlockchainUpdated b
    --                         newfixed
    = BlockInserted (Lively b) [b]
    | BLockInsertedLinksToRoot (Lively b)
    | FutureBlock (Future b)
    | BlockAlreadyInserted
    | BlockInvalid
-- | BlockNotLinked

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

class LinksToChain b where
    prevRef :: (b -> BlockReference)
    thisRef :: (b -> BlockReference)

instance LinksToChain Block where
    prevRef = blockPreviousHash
    thisRef = shash256 . Right . blockHeader

instance LinksToChain BlockHeader where
    prevRef = previousHash
    thisRef = shash256 . Right

-- Updates utxoPool with given new fixed blocks.
-- Pass newfixed in the order returned from updateWithBlock.
newfixed2UTXOPoolUpdate :: [Block] -> UTXOPool -> UTXOPool
newfixed2UTXOPoolUpdate newfixed utxoPool = collectUTXOs utxoPool (reverse newfixed)

newfixed2SpentTXOSet :: [Block] -> Set Output
newfixed2SpentTXOSet = map (concatmap outputs . transactions)

-- If a block can be appended to a block in LivelyBlocks, 
-- then append it, prune branches and move some of the older blocks from Lively to Fixed
updateWithBlockGeneric ::
    (LinksToChain b, Ord b) =>
       (TargetHash -> b -> Bool)
    -> ([(Tree b, Maybe (Zipper b))]
                  -> [(Tree b, Maybe (Zipper b))]
                  -> Zipper b
                  -> BlockchainUpdated b)
    -- -> (b -> BlockReference)     -- get reference to previous block
    -- -> (b -> BlockReference)     -- get this block reference
    -> ForkMaxDiff                   -- constant specifying which forks can be safely discarded - the ones strictly shorter than maxdiff
    -> TargetHash                    -- constant specifying mining difficulty
    -- -> UTXOPool                      -- pool of utxos from FixedBlocks
    -> b                         -- block to be validated and inserted into LivelyBlocks
    -> Lively b                  -- recent chains
    -- -> Fixed b                   -- older chain
    -> Future b                  -- blocks that might link to blocks we haven't received yet
    -> BlockchainUpdated b                      -- blockchain updated with the block
updateWithBlockGeneric todo1 todo2 (ForkMaxDiff maxdiff) target newblock lb@(Lively {root ,forest}) (Future future) =
    -- Does block link directly to root?
    if prevRef newblock == root then
        -- Is it already present?
        if any ((== thisRef newblock) . (\case Tree b _ -> thisRef b) ) forest then
            BlockAlreadyInserted
        else
            -- Is it valid?
            if todo1 target newblock then
                BLockInsertedLinksToRoot (lb {forest=newTree newblock : forest})
            else
                BlockInvalid
    else
        -- TODO: improve this junk algorithm
        -- Find a node in LivelyBlocks with given hash referenced by newblock
        case break (isJust . snd) $ map (id &&& linkToChain newblock) forest of

            -- New block doesn't append to any known recent block - add it to FutureBlocks.
            -- TODO: Manage FutureBlocks.
            (_, []) -> FutureBlock . Future $ Map.alter (maybe (Just $ Set.singleton newblock) (Just . Set.insert newblock)) (prevRef newblock) future

            -- new block appends to blockchain
            (ts1, (_, Just zipper) : ts2) ->
                if blockAlreadyInserted newblock zipper then
                    BlockAlreadyInserted
                else
                    -- calculate UTXOPool in a moment in blockchain where the new block appends 
                    todo2 ts1 ts2 zipper
            (_, (_, Nothing) : _) -> error "Break on (isjust . snd) - doesn't happen"

updateWithBlockHeader :: ForkMaxDiff                   -- constant specifying which forks can be safely discarded - the ones strictly shorter than maxdiff
                 -> TargetHash                    -- constant specifying mining difficulty
                 -> BlockHeader                         -- block to be validated and inserted into LivelyBlocks
                 -> Lively BlockHeader                   -- recent chains
                --  -> Fixed BlockHeader           -- older chain
                 -> Future BlockHeader          -- blocks that might link to blocks we haven't received yet
                 -> BlockchainUpdated BlockHeader                       -- blockchain updated with the block
updateWithBlockHeader forkdiff@(ForkMaxDiff maxdiff) target newblock lb@(Lively {root ,forest}) ft@(Future future) =
    updateWithBlockGeneric
        validateNonce
        todo
        forkdiff target newblock lb ft
    where
        todo ts1 ts2 zipper =
            -- calculate UTXOPool in a moment in blockchain where the new block appends 
            -- let utxoPool'           = collectUTXOs utxoPool $ pathFromRoot zipper in

            if  validateNonce target newblock then
                -- We append a new block with all the blocks waiting in the FutureBlocks that append to it.
                -- We recursively create a tree of blocks from FutureBlocks and put it into Livelyblocks in a place given by zipper.
                let newtree = insertFuturesHeaders target future newblock in
                -- we put the tree with inserted blocks back into the list
                let newforest = map fst ts1 ++ [fromZipper $ insertTreeHere newtree zipper] ++ map fst ts2 in
                -- Said tree is hung (hanged?) in the LivelyBlocks tree and a resulting tree is pruned and old blocks are moved to FixedBlocks.
                let (newfixed, lively) = fixBlocks maxdiff $ prune maxdiff newforest
                in BlockInserted (Lively (maybe root thisRef (safeHead newfixed)) lively) newfixed
            else
                BlockInvalid


updateWithBlock :: ForkMaxDiff                   -- constant specifying which forks can be safely discarded - the ones strictly shorter than maxdiff
                 -> TargetHash                    -- constant specifying mining difficulty
                 -> UTXOPool                      -- pool of utxos from FixedBlocks
                 -> Block                         -- block to be validated and inserted into LivelyBlocks
                 -> LivelyBlocks                  -- recent chains
                --  -> FixedBlocks                   -- older chain
                 -> FutureBlocks                  -- blocks that might link to blocks we haven't received yet
                 -> BlockchainUpdated Block                       -- blockchain updated with the block
updateWithBlock forkdiff@(ForkMaxDiff maxdiff) target utxoPool newblock lb@(Lively {root ,forest}) ft@(Future future) =
    updateWithBlockGeneric
        (\t b -> fst $ validateBlock t utxoPool b)
        todo
        forkdiff target newblock lb ft
    where
        todo ts1 ts2 zipper =
            -- calculate UTXOPool in a moment in blockchain where the new block appends 
            let utxoPool'           = collectUTXOs utxoPool $ pathFromRoot zipper in
            -- validate the block
            let (valid, utxoPool'') = validateBlock target utxoPool' newblock   in

            if valid then
                -- We append a new block with all the blocks waiting in the FutureBlocks that append to it.
                -- We recursively create a tree of blocks from FutureBlocks and put it into Livelyblocks in a place given by zipper.
                let newtree = insertFutures target future utxoPool'' newblock in
                -- we put the tree with inserted blocks back into the list
                let newforest = map fst ts1 ++ [fromZipper $ insertTreeHere newtree zipper] ++ map fst ts2 in
                -- Said tree is hung (hanged?) in the LivelyBlocks tree and a resulting tree is pruned and old blocks are moved to FixedBlocks.
                let (newfixed, lively) = fixBlocks maxdiff $ prune maxdiff newforest
                in BlockInserted (Lively (maybe root blockRef (safeHead newfixed)) lively) newfixed
            else
                BlockInvalid



-- This is left unchanged for now, can be used unchanged in testing, why not.

-- If a block can be appended to a block in LivelyBlocks, 
-- then append it, prune branches and move some of the older blocks from Lively to Fixed
updateWithBlock1 :: ForkMaxDiff                   -- constant specifying which forks can be safely discarded - the ones strictly shorter than maxdiff
                 -> TargetHash                    -- constant specifying mining difficulty
                 -> UTXOPool                      -- pool of utxos from FixedBlocks
                 -> Block                         -- block to be validated and inserted into LivelyBlocks
                 -> LivelyBlocks                  -- recent chains
                 -> FixedBlocks                   -- older chain
                 -> FutureBlocks                  -- blocks that might link to blocks we haven't received yet
                 -> BlockchainUpdated Block                       -- blockchain updated with the block
updateWithBlock1 (ForkMaxDiff maxdiff) target utxoPool newblock lb@(Lively {root ,forest}) fb@(Fixed fixed) (Future future) =
    -- Does block link directly to root?
    if blockPreviousHash newblock == root then
        -- Is it already present?
        if any ((== shash256 (blockHeader newblock)) . (\case Tree b _ -> shash256 (blockHeader b)) ) forest then
            BlockAlreadyInserted
        else
            -- Is it valid?
            if fst $ validateBlock target utxoPool newblock then
                BLockInsertedLinksToRoot (lb {forest=newTree newblock : forest})
            else
                BlockInvalid
    else
        -- TODO: improve this junk algorithm
        -- Find a node in LivelyBlocks with given hash referenced by newblock
        case break (isJust . snd) $ map (id &&& linkToChain newblock) forest of

            -- New block doesn't append to any known recent block - add it to FutureBlocks.
            -- TODO: Manage FutureBlocks.
            (_, []) -> FutureBlock . Future $ Map.alter (maybe (Just $ Set.singleton newblock) (Just . Set.insert newblock)) (blockPreviousHash newblock) future

            -- new block appends to blockchain
            (ts1, (_, Just zipper) : ts2) ->
                if blockAlreadyInserted newblock zipper then
                    BlockAlreadyInserted
                else
                    -- calculate UTXOPool in a moment in blockchain where the new block appends 
                    let utxoPool'           = collectUTXOs utxoPool $ pathFromRoot zipper in
                    -- validate the block
                    let (valid, utxoPool'') = validateBlock target utxoPool' newblock   in

                    if valid then
                        -- We append a new block with all the blocks waiting in the FutureBlocks that append to it.
                        -- We recursively create a tree of blocks from FutureBlocks and put it into Livelyblocks in a place given by zipper.
                        let newtree = insertFutures target future utxoPool'' newblock in
                        -- we put the tree with inserted blocks back into the list
                        let newforest = map fst ts1 ++ [fromZipper $ insertTreeHere newtree zipper] ++ map fst ts2 in
                        -- Said tree is hung (hanged?) in the LivelyBlocks tree and a resulting tree is pruned and old blocks are moved to FixedBlocks.
                        let (newfixed, lively) = fixBlocks maxdiff $ prune maxdiff newforest
                        in BlockInserted (Lively (maybe root blockRef (safeHead (newfixed ++ fixed))) lively) newfixed
                    else
                        BlockInvalid
            (_, (_, Nothing) : _) -> error "Break on (isjust . snd) - doesn't happen"

insertFuturesHeaders :: TargetHash
                -> Map.Map BlockReference (Set.Set BlockHeader)    -- future blocks, keys are blockPreviousHash'es
                -> BlockHeader                         -- Block to put in the root 
                -> Tree BlockHeader                    -- Tree made of blocks from FutureBlocks Map rooted in the given block
insertFuturesHeaders target futures block = case Map.lookup (thisRef block) futures of
    Just blocks ->

        Tree block $ foldl' (
            \bs b ->
                if validateNonce target b then
                    insertFuturesHeaders target futures b : bs
                else
                    bs
            ) [] blocks

    Nothing -> Tree block []


-- We take zipper focused on newly added block. Check whether there's block waiting in the futures to be appended here.
-- If so - append it and append blocks from futures recursively. 
insertFutures :: TargetHash
                -> Map.Map BlockReference (Set.Set Block)    -- future blocks, keys are blockPreviousHash'es
                -> UTXOPool                      -- utxoPool up to the root block including
                -> Block                         -- Block to put in the root 
                -> Tree Block                    -- Tree made of blocks from FutureBlocks Map rooted in the given block
insertFutures target futures utxoPool block = case Map.lookup (blockRef block) futures of
    Just blocks ->

        Tree block $ foldl' (
            \bs b ->
                let (valid, utxoPool') = validateBlock target utxoPool b in
                if valid then
                    insertFutures target futures utxoPool' b : bs
                else
                    bs
            ) [] blocks

    Nothing -> Tree block []


pathFromRootA :: Place a -> [a]-> [a]
pathFromRootA (Root bl) acc               = bl : acc
pathFromRootA (Brother parent _ bl _) acc = pathFromRootA parent (bl : acc)

-- returns a list of blocks on a path starting from root and ending on given Zipper
pathFromRoot :: Zipper a -> [a]
pathFromRoot (Zipper _ pl) = pathFromRootA pl []

collectUTXOs :: UTXOPool -> [Block] -> UTXOPool
collectUTXOs = foldl' insert2map
    where

    -- size(pool) = n, size(block) = m
    -- does m insertions working in O(mlogn)
    insert2map :: UTXOPool -> Block -> UTXOPool
    insert2map (UTXOPool pool) (Block {transactions=txs, coinbaseTransaction=coinbaseTransaction}) = UTXOPool $
        foldl'
            (\pl (UTXO txid vout out) -> Map.insert (txid, vout) out pl)
            pool
            (coinbaseGetNewUTXOs coinbaseTransaction <> concatMap txGetNewUTXOs txs)


-- Invariant:
-- LivelyBlocks is a forest of blocks appending (directly or indirectly) to a given BlockReference.
-- This way we account for the early states of appending to genesis.
-- PreviousBlockReference's of the roots of the trees in forest equal "root".
-- UTXOPool is set of txs of all the FixedBlocks.
-- Blocks in LivelyBlocks are all uncertain.

-- we will be appending new blocks, so let the order be:
-- head fixedBlocks == newest block
-- last fixedBlocks == first block after genesis
newtype Fixed b = Fixed {getFixedBlocks :: [b]}
    deriving (Show, Generic)
data Lively b = Lively { root :: BlockReference, forest :: [Tree b]}
    deriving (Show, Generic)

drawLively :: (b -> String) -> Lively b -> String
drawLively b2str Lively {root=root, forest=forest} =
    "root " <> show root <> ":\n"
    <> drawMyForest (map (fmap b2str) forest)

-- Set uses our shash256 hashing for ordering. HashSet can't be used because Hashable instance demands hash with salt implementation.
newtype Future b = Future { getFutureBlocks :: Map.Map BlockReference (Set.Set b) }
    deriving (Show)

type FixedBlocks = Fixed Block

type LivelyBlocks = Lively Block
    -- deriving (Show, Generic)

-- Set uses our shash256 hashing for ordering. HashSet can't be used because Hashable instance demands hash with salt implementation.
type FutureBlocks = Future Block
    -- deriving (Show)


instance ToJSON (Fixed Block)
instance FromJSON (Fixed Block)

instance ToJSON (Lively Block)
instance FromJSON (Lively Block)

-- type not in use but this is blockchain: 
-- data Blockchain
--     = Blockchain FixedBlocks LivelyBlocks Genesis

-- Searches block tree looking for a Block/Blockheader that links to a matching previousHash,
-- returns a zipper focused on this block - the new block should be inserted here as a child.
-- 
linkToChain ::
    LinksToChain b =>
    b
    -> Tree b
    -> Maybe (Zipper b)
linkToChain b t =
    let prevHash = prevRef b in
    let pred = (==) prevHash . thisRef . getElem
    in find pred $ dfs t

-- Finds a block in a Tree whose hash is referenced in a provided block.
-- If also provided block is not already a children of a found block, the block is inserted as a child.
-- Otherwise returns Nothing.
insertToChain :: Block -> Tree Block -> Maybe (Tree Block)
insertToChain b t =
    linkToChain b t <&> \z@(Zipper ts _) ->
        -- insert if it's not present already
        -- TODO: could be improved with bin-search, would need to change list for vector, probably not worth
        let bHash = shash256 . blockHeader $ b in
        fromZipper $ if all (\(Tree bb _) -> shash256 (blockHeader bb) /= bHash) ts then
            insertHere b z
        else
            z

-- checks whether given block is already inserted as a child of focused node in zipper
-- check is achieved by comparing blockheader hashes
blockAlreadyInserted :: LinksToChain b => b -> Zipper b -> Bool
blockAlreadyInserted block (Zipper ts _) =
    let hash = thisRef block
    in  any (\(Tree bb _) -> thisRef bb == hash) ts

data Tree a = Tree a [Tree a] deriving (Show, Functor, Generic) -- not empty

instance ToJSON a => ToJSON (Tree a)
instance FromJSON a => FromJSON (Tree a)

tree2tree :: Tree a -> DTree.Tree a
tree2tree (Tree a ts) = DTree.Node a (map tree2tree ts)
drawMyTree = DTree.drawTree . tree2tree
drawMyForest = DTree.drawForest . map tree2tree
drawBlockheights :: [Tree Block] -> String
drawBlockheights = drawMyForest . map (fmap (show . blockBlockHeight))

-- Focused tree.
-- Represents a location in a Tree, a node. Tree can be reconstructed from Zipper. 
--                  children ↓  , ↓ element kept at the node + "surroundings" 
data Zipper a = Zipper [Tree a] (Place a)
    deriving Show

-- element kept at the node + surroundings 
data Place a = Root a                                 -- describes a root node with element a
             | Brother (Place a) [Tree a] a [Tree a]  -- describes a children of a node described in (Place a), with element a, and given left/right siblings.
             deriving (Show, Functor)
            --  deriving (Show, Functor, Generic)

newTree :: a -> Tree a
newTree a = Tree a []

getElem :: Zipper a -> a
getElem (Zipper _ (Root a)) = a
getElem (Zipper _ (Brother _ _ a _)) = a

goDown :: Zipper a -> Maybe (Zipper a)
goDown (Zipper [] _) = Nothing
goDown (Zipper ((Tree a ts) : cs) p) = Just $ Zipper ts (Brother p [] a cs)

goRight :: Zipper a -> Maybe (Zipper a)
goRight (Zipper _ (Root _)) = Nothing
goRight (Zipper _ (Brother _ _ _ [])) = Nothing
goRight (Zipper ts (Brother f l a (Tree e cs : rs))) = Just $ Zipper cs (Brother f (Tree a ts : l) e rs)

toZipper :: Tree a -> Zipper a
toZipper (Tree a ts) = Zipper ts (Root a)

fromZipper :: Zipper a -> Tree a
fromZipper (Zipper ts (Root a)) = Tree a ts
fromZipper (Zipper ts (Brother f l a r)) = fromZipper $ Zipper (reverse l ++ [Tree a ts] ++ r) f

modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Zipper ts (Root a)) = Zipper ts (Root $ f a)
modify f (Zipper ts (Brother pl ls a rs)) = Zipper ts (Brother pl ls (f a) rs)

insertHere :: a -> Zipper a -> Zipper a
insertHere a (Zipper ts b) = Zipper (Tree a [] : ts) b

insertTreeHere :: Tree a -> Zipper a -> Zipper a
insertTreeHere t (Zipper ts b) = Zipper (t : ts) b

-- Deletes a tree rooted where we're focused on. Focus jumps to parent.
-- Returns Nothing if it's the root that is deleted and nothing is left. 
-- deleteHere :: Zipper a -> Maybe (Zipper a)
-- deleteHere (Zipper ts (Root _)) = Nothing 
-- deleteHere (Zipper ts (Brother pl ls _ rs)) = Just $ Zipper (reverse ls ++ rs) pl

-- returns a list of tree's focused (zippers) on nodes in dfs order
dfs :: Tree a -> [Zipper a]
dfs = dfsz . toZipper
    where
        dfsz :: Zipper a -> [Zipper a]
        dfsz z = z : concatMap dfsz (go (goDown z))

        go Nothing = []
        go (Just zz) = zz : go (goRight zz)
