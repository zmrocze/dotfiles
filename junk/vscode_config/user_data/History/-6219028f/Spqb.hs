-- {-# LANGUAGE GADTs #-}
module Hsh.Main  where

import qualified Language.Haskell.Interpreter as I
import Language.Haskell.Interpreter (parens)
import Data.Data (Typeable)
import System.Environment (getArgs)
import Text.Pretty.Simple (pPrint)
import System.Exit (exitFailure)
-- This module is imported also by interpreter. This is fishy but works.
import Hsh.Constructors ( Val(..), Arg(..), Allowed(..) )
import Data.Traversable (for)
import Data.Either (partitionEithers)
import Control.Monad.Extra (findM)
import Data.Either.Extra (mapRight)
import HshPrelude (
    langExtensions
  , preludePath
  , constructorsPath)

arg2str :: Arg v -> (String -> v)
arg2str (InStr f) = f
arg2str (Lines f) = f . lines
arg2str (WordLines f) = f . map words . lines

-- val2str :: Arg Val -> (String -> String)
-- val2str f str = case arg2str f str of 
--   OutStr s -> s
--   UnLines ss -> unlines ss
--   UnWordLines sss -> unlines $ map unwords sss

val2str :: Val -> String
val2str (OutStr s) = s
val2str (UnLines ss) = unlines ss
val2str (UnWordLines sss) = unlines $ map unwords sss

toIO :: Allowed -> IO ()
toIO (JustIO io) = io
toIO (Func f) = interact $ val2str . arg2str f
toIO (Action g) = do
  str <- getContents >>= fmap val2str . arg2str g
  putStr str
toIO (IOFunc h) = getContents >>= arg2str h

argConstrStrs = ["InStr", "Lines", "WordLines"]
valConstrStrs = ["OutStr", "UnLines", "UnWordLines"]

-- f :: [String] -> String 
-- f = undefined 
-- foo = (Func . Lines) (OutStr . f)

-- g :: [[String]] -> IO [String] 
-- g = undefined 
-- bar = (Action . WordLines) (fmap UnLines . g)
-- 
-- List of functions surrounding expression in constructors. Assumes expr is in parens.
surroundExpr :: [String -> String]
surroundExpr =
  -- func type   :: Arg -> Val
     [ \expr -> parens ("Func . " <> arg) <> parens (val <> " . " <> expr) | arg <- argConstrStrs, val <- valConstrStrs ]
  -- action type :: Arg -> IO Val 
  <> [ \expr -> parens ("Action . " <> arg) <> parens ( "fmap " <> val <> " . " <> expr ) | arg <- argConstrStrs, val <- valConstrStrs ]
  -- simple io type :: IO ()
  <> [\expr -> "JustIO " <> expr]
  -- io function :: Arg -> IO () 
  <> [\expr -> parens ("IOFunc . " <> arg) <> expr | arg <- argConstrStrs]

constructorsPath = "/home/zmrocze/code/haskell/hsh/src/Hsh/Constructors.hs"

tryRun :: I.MonadInterpreter m => String -> m (Maybe String)
tryRun expr =
  let expr1 = parens expr in do
    I.loadModules [preludePath, constructorsPath]
    I.set [I.languageExtensions I.:= langExtensions, I.installedModulesInScope I.:= True]
    I.setImportsQ [
      ("HshPrelude", Nothing), 
      ("Constructors", Nothing)
      ] <> extraQualifiedImports
    findM I.typeChecks (map ($ expr1) surroundExpr)

-- Interpret functionStr and apply the result on stdinStr. 
run :: String -> IO ()
run expr = do
  ef <- I.runInterpreter $ do
    m <- tryRun expr
    case m of
      -- meant to trigger evaluation error and show some hints in error msg
      Nothing -> Left <$> I.typeChecksWithDetails expr
      -- here it should work
      Just s -> Right <$> I.interpret s (I.as :: Allowed)

  case ef of
    (Right (Right f)) -> toIO f
    (Right (Left e))  -> pPrint (mapRight (\s -> "Infered: " <> s <> "\nDon't know what to do with it." ) e) >> exitFailure 
    (Left err)        -> pPrint err >> exitFailure

main = do
  fstr <- fmap concat getArgs
  run fstr
