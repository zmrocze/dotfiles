module Main (main) where
import Data.String (fromString)
import Data.Word8
-- import Numeric (showBin)
import Plutus.V1.Ledger.Bytes (LedgerBytes(getLedgerBytes), fromHex)
import Plutus.V1.Ledger.Api (fromBuiltin, BuiltinByteString, Script, LedgerBytes (LedgerBytes), toBuiltin, Data (..), toData, BuiltinData)
import Data.ByteString (ByteString)
import Prelude
import Codec.Serialise (deserialise, serialise)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Ledger (Script(unScript), applyArguments, fromCompiledCode)
import UntypedPlutusCore.Core (progTerm)
import Control.Lens ((^.))
import Data.Word (Word8)
import qualified Data.ByteString as B
import qualified PlutusTx
import PlutusTx (CompiledCode)
import Control.Arrow ((>>>), Arrow ((***)))
import qualified UntypedPlutusCore as UPLC
import qualified PlutusCore as PLC
import qualified Flat
import Data.Int (Int64, Int32, Int16)
import Cardano.Api (BlockHeader, PlutusScriptV1, deserialiseFromTextEnvelope, TextEnvelope (TextEnvelope), HasTypeProxy (proxyToAsType), readFileTextEnvelope, PlutusScript, SerialiseAsRawBytes (serialiseToRawBytes, deserialiseFromRawBytes), PlutusScriptV2, HasTextEnvelope (textEnvelopeDefaultDescr), writeFileTextEnvelope)
import Data.Data (Proxy(Proxy))
import Control.Exception (throwIO)
import System.Exit (die)
showHex :: ByteString -> String
showHex = show . LedgerBytes . toBuiltin
-- Awlayssucceeds v1
script0a = toScript "4d01000033222220051200120011"

script0a_haskell_unit = toScript "55010000333222220051200120014c0103d879800001"
script0a_wasm_unit = toScript    "56010000333222220051200120014c0104d8799fff0001"

script0a_applied45 = toScript "58180100003333222220051200120014c10104004c0101050001" -- ctl server

script2aa = toScript "534d0100330034c104d8799fff004c0101050001"

scriptalways_succeed_4_5 = toScript "0100003333222220051200120014c10104004c0101050001" -- bad, js
applied_4_5_rust_bytes = B.pack [88, 24, 1, 0, 0, 51, 51, 34, 34, 32, 5, 18, 0, 18, 0, 20, 193, 1, 4, 0, 76, 1, 1, 5, 0, 1]
-- scriptalways_succeed_4_5_rust = deserialise $ fromStrict applied_4_5_rust_bytes


res_words_2a :: [Word8]
res_words_2a = [83, 77, 1, 0, 51, 0, 52, 193, 4, 216, 121, 159, 255, 0, 76, 1, 1, 5, 0, 1]
res_bytes_2a = LedgerBytes $ toBuiltin $ B.pack res_words_2a

xxxx = "58180100003333222220051200120014c10104004c0101050001"

toScript :: String -> Script
toScript = deserialise . fromStrict . fromBuiltin . getLedgerBytes . fromString
hexToBs = fromBuiltin . getLedgerBytes . fromString
script0 = toScript "540100002225333573466e1cdd6801a40a82930b01"
script1 = toScript "5821010000332225333573466e1cdd6801a40a82930b260103d87980004c0101050001"
script2 = toScript "540100330024c104d8799fff004c0101050001"

-- scripts = [script0, script1, script2, script0a, script1a, script2a]
scripts = [script0a, script0a_haskell_unit, script0a_wasm_unit]

getLambda scr = unScript scr ^. progTerm

-- [Unit] from rust
unitBytesRust = B.pack [159, 192, 159, 255, 255]
unitBytesJs = fromBuiltin . getLedgerBytes . fromString $ "9fd87980ff"
unt = Constr 0 []
pltmap = Map [(I 1, unt), (I 2, pltlist)]
pltlist = List [I 0, I 1, I 2]
pltpair = toData (0 :: Integer, 1 :: Integer)
plttrue = toData True
pltbig = Constr 5 [pltmap, Constr 5 [Map [(pltmap, plttrue), (pltlist, pltmap), (List [], List [I 1])], unt, pltpair]]
pltemptylists = Constr 5 [List [], List [I 1], Map [], Map [(I 1, unt), (I 2, Constr 2 [I 2])]]
pltshortbs = B (B.replicate 32 105)
pltlongbs = B (B.replicate 67 105)
pltemptybs = B B.empty
pltData = [unt, pltmap, pltlist, pltbig, pltemptylists, pltshortbs, pltlongbs, pltemptybs]

-- alonzoheaderHex = "828f0000f658206907a24fc62affc690ae4c61044d4874c7c013333a5e94d4669a69dba7a0201f582029b2880af81ef578e7506bd7e840db694b08647dc803ee99b25ae9dd3c4e221082584057768126fd510b5ebf4555725cc04cb8d582c865d41c52aa26fc78583a384820de2d561dc864650a5d70f476eeaaee1472ed6d17cde3d40bbba835cbc3726acf58506b77911a16a65a8873c86dafa4017227bbcc56b7ddbee2a12d28366f2b9e6aa0212591d9a47d71cd82e6749027a69b66655b2cedb8277ead6573db8e4c0d627429f6bc56c5be2a8332854edb1ed35d00825840644696bd766ee07a357559282ffa3001b70115a78c4f20a011a611a4a2b23621039f462913c0deb95e50d3652b1897f06012cd42c1082f213b13e13e547d08b15850bc20d89f10c103836d264284e35b8affc6defc7cfece3105b9b9e5e63025bada42272ea8e43ab65a9371175566e51fed8a3adb803426e5f8a896bbb93937e5f2562df4b144d3eef57d8eb364bd77250b04582029571d16f081709b3c48651860077bebf9340abb3fc7133443c54f1f5a5edcf1582012963cf7b37ed0cbcd39f8d45c3bc7911137be765944daf77288e369dfaf3adb00005840e2e734e33cc23cd86a11180b9f6e71eecdfef519ec0b067f0b2703a2dc4edafb652c77544c7f383e0f3c8355c7cbbefe35933c6ffa7aedb7861fee5065793a0707025901c029fbfaadbf4656c60ed5e1a772b4879cf4fb04b87c375ef9681849faec7766b7a11b580541945aecaeeb84e27afcf1f64e2dc7d6299ee4cc2548be0450ae060ddef15249e0ec1779047bfc7483b8422dd3275bb48aa6a187dc8d75b07e727628423165c40dd6a995aa4378f82802aace6cd008cb710ea11f614d4818eb5075f51f74c2310ab48176f2aa1adcc1dbef0562b944ab809b6fa1380133ab38776e52a34ff3909c05bb8b3ae4118262f430679d0d58d0323eef159407f3a430c4baa9f6cd78fbab647abfde4c01a313389cfe6cd6bf8634142fe81af6c638c7dc77d677a9894bb6f444815b4aa6c2a24da64b702f46e58ce8fa93b0061fa2f412ba375831607fb315f6d7823b64e8af135c666d54315b5977484494eb97db70da0398e93494988cb7b97d679cac4ccce6bd5b22e3b687fbc62e613aa4cdb2e1bea64715e7da5c1c8c839d629e07274bca48736f1d97fcf29eda46c4a8aef14f84fb7361ae32324a562f79070eeee0794c1751da919829971797b2683007c9bba4a9903fc60eed1cff1cd755ee98b0099db8aebbec26a3ff918f6730dcfbe5f24de6edcd3aeff3b2f62a14c01d9a2a33e232cffc95f26abb20e6bb9fc461a176adca50"
-- alonzoheader :: BlockHeader
-- alonzoheader = deserialise alonzoheaderHex
alwaysSucceeds :: BuiltinData -> BuiltinData -> BuiltinData -> ()
alwaysSucceeds _ _ _ = ()

alwaysSucceedsCompiled :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
alwaysSucceedsCompiled = $$(PlutusTx.compile [|| alwaysSucceeds ||])

-- toCardanoApiScript :: CompiledCode a -> ByteString
serialiseScript = toStrict . serialise . fromCompiledCode

-- alwaysSucceedsHex = "480100002221200101"

scr1a = toScript "581b01000032225333573466e1cdd6801a40a82930b26103d879800001"
scr1b = toScript "581c01000032225333573466e1cdd6801a40a82930b26104d8799fff0001"
scr2a = toScript "581d01000032225333573466e1cdd6801a40a82930b26105a101d879800001"
scr2b = toScript "581f01000032225333573466e1cdd6801a40a82930b26107bf01d8799fffff0001"
scr4a = toScript "581f01000032225333573466e1cdd6801a40a82930b26107a20507424d5f080001"
scr4b = toScript "582001000032225333573466e1cdd6801a40a82930b26108bf0507424d5f08ff0001"

-- x = case getLambda scr1a of 
--     UPLC.Apply _ _ (UPLC.Constant _ (PLC.Some (PLC.ValueOf a _))) -> a
--     _ -> undefined

-- uplc_unit = (UPLC.Constant _ (PLC.Some (PLC.ValueOf a _))

-- main :: IO ()
-- main = do
--   print $ "unit serialized" <> showHex (toStrict $ serialise $ toData ())
--   print $ "should be: " <> show (applyArguments script0a [toData ()])
--   mapM_ (print . unScript) scripts

--   print $ script0a_haskell_unit == script0a_wasm_unit

--   let x = serialiseScript alwaysSucceedsCompiled
--   print $ showHex x 
--   print $ getLambda $ fromCompiledCode alwaysSucceedsCompiled

--   print "allEqual?"
--   mapM_ (print) [ scr1a == scr1b , scr2a == scr2b, scr4a == scr4b] 
--   mapM_ (print . uncurry (==) . (let x = serialise in x *** x)) [ (scr1a , scr1b)  , (scr2a , scr2b) , (scr4a , scr4b), (script1a, script2a) ] 


convertToBase :: Word8 -> Integer -> String
convertToBase b n
    | n < 0              = '-' : convertToBase b (-n)
    | n < fromIntegral b = [(['0'..'9'] ++ ['A' .. 'Z']) !! fromIntegral n]
    | otherwise          = let (d, m) = n `divMod` fromIntegral b in convertToBase b d ++ convertToBase b m

hexToBinStr :: String -> [Char]
hexToBinStr = concatMap (convertToBase 2 . toInteger) . B.unpack . fromBuiltin . getLedgerBytes . fromString

-- main = do 
--   print $ hexToBinStr $ showHex unitBytesRust
--   print $ hexToBinStr $ showHex unitBytesJs

--   print $ hexToBinStr $ "581b01000032225333573466e1cdd6801a40a82930b26103d879800001"
--   print $ hexToBinStr $ "581c01000032225333573466e1cdd6801a40a82930b26104d8799fff0001"
--   print $ hexToBinStr $ "9fd87980ff"

type Program = UPLC.Program UPLC.DeBruijn PLC.DefaultUni PLC.DefaultFun ()

-- main = do 
--   print $ scr1a == scr1b
--   print $ getLambda scr1a
--   let a_bytes = serialise scr1a
--   let a_bs :: ByteString = deserialise a_bytes
--   let a_scr :: Either Flat.DecodeException Program = Flat.unflat a_bs
--   print a_scr
-- 9fd87980ff
alwaysSucceedsCTL = toScript                "4d01000033222220051200120011"
alwaysSucceedsJS_unit = toScript            "56010000333222220051200120014c0104d8799fff0001"
                                        --   56010000333222220051200120014c0104d8799fff0001
alwaysSucceedsHaskellServer_unit = toScript "55010000333222220051200120014c0103d879800001"
alwaysSucceedsHaskell_unit = applyArguments alwaysSucceedsCTL [toData ()]
alwaysSucceedsHaskell_somedata = let
  dt = Constr 121 []
    in applyArguments alwaysSucceedsCTL [dt]
alwaysSucceedsHaskell_unit121 = toScript "56010000333222220051200120014c0104d90572800001"
todata_unit = toData [()]

-- scripts = [ alwaysSucceedsScript, alwaysSucceedsScriptV2, only42Script ]
paramss =
  [ ([], "no-args")
  , ([ un ], "unit")
  , ([ n 7, un, List [ un, bytes ], longBytes, Map [ (n 5, n 7), (bytes, n 8) ],
      Constr 102 [n 7, List [ un, bytes, longBytes ]],
      Constr 5 [List [], List [I 1], Map [], Map [(I 1, un), (I 2, Constr 2 [I 2])]]
    ], "big-arg")
  ]
  where
  n :: Integer -> Data
  n k = toData k
  un = toData ()
  bytes = B $ hexToBs "4d5f"
  longBytes = B $ hexToBs $ foldl (\x y -> x <> y) "" $
    replicate 65 "4d"

printDataCommented d = putStrLn $ "// " <> show d <> "\n" <> (show . showHex . toStrict . serialise $ d)
printData d = putStrLn $ (show . showHex . toStrict . serialise $ d)

-- main = do 
--   print $ unScript alwaysSucceedsCTL
--   let scripts = [alwaysSucceedsJS_unit, alwaysSucceedsHaskellServer_unit, alwaysSucceedsHaskell_unit, alwaysSucceedsHaskell_somedata]
--   mapM_ (print . getLambda) scripts
--   print "hex:"
--   mapM_ (print . showHex . toStrict . serialise) scripts
--   print $ showHex $ toStrict $ serialise todata_unit
--   let a :: Data =  deserialise $ serialise todata_unit
--   print a
--   putStr "datas: \n\n"
--   mapM_ printDataCommented pltData

  -- print "flats for alwayssucceed_haskell_unit"
  -- print $ showHex $ Flat.flatRaw $ unScript alwaysSucceedsHaskellServer_unit
  -- print $ showHex $ Flat.flat $ unScript alwaysSucceedsHaskellServer_unit

loadScript lang fp = do
  escr <- readFileTextEnvelope  (proxyToAsType $ lang) ("plutus-scripts/" <> fp <> ".plutus")
  scr <- either (die . show) pure escr
  let bs = showHex $ serialiseToRawBytes scr
  putStrLn bs
  let script :: Script = deserialise $ fromStrict $ serialiseToRawBytes scr
  print $ unScript script
  pure script

main = do

  -- let res = map (uncurry applyArguments) ((,) <$> scrs <*> paramss)
  -- mapM (putStrLn . showHex . toStrict . serialise) res
  let yy = xx v1 v1scriptPaths a <> xx v2 v2scriptpaths a
  _
  where

    xx lang paths f = map (uncurry (f lang)) $
      (,) <$> paths  <*> paramss

    a lang scrpath (args, pname) = do
      scr <- loadScript lang scrpath
      let res1 = applyArguments scr args
      res <- maybe (die "fuck") id $ deserialiseFromRawBytes (proxyToAsType lang) $ toStrict $ serialise res1
      r <- writeFileTextEnvelope
        ("plutus-scripts/applied/" <> scrpath <> "-" <> pname <> ".plutus")
        Nothing res
      either (die . show) pure r

    v1 = Proxy @(PlutusScript PlutusScriptV1)
    v2 = Proxy @(PlutusScript PlutusScriptV2)
    v1scriptPaths =
      [ "always-fails"
      , "include-datum"
      , "one-shot-minting"
      , "redeemer1-validator"]

    v2scriptpaths =
      ["always-succeeds-v2",
       "one-shot-minting-v2"
        , "check-datum-is-inline"
      ]
