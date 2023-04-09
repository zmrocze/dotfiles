{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}

module Plutus.Contracts.MyLittleScript where

import PlutusTx
import Data.Function ((&))
import Ledger (Script(unScript), fromCompiledCode)
import Control.Lens ((^.))
import UntypedPlutusCore (progTerm, DeBruijn, DefaultUni, DefaultFun)
import qualified UntypedPlutusCore.Core.Type
import Flat (flat)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import System.IO (openFile, IOMode (WriteMode), withFile)
import PlutusTx.Prelude

-- integerOne :: CompiledCode ()
-- integerOne :: UntypedPlutusCore.Core.Type.Term
--   DeBruijn
--   DefaultUni
--   DefaultFun
--   ()
-- integerOne = $$(compile
--         [|| (() :: ()) ||]
--     ) & fromCompiledCode & unScript & (^. progTerm)

    -- & (^. progTerm)

-- integerOne :: _
unitCompiled :: CompiledCode ()
unitCompiled = $$(compile
        [|| (() :: ()) ||]
        )

unitFlat :: ByteString
unitFlat = flat unitCompiled

{-# INLINEABLE sillyBool #-}
sillyBool :: Bool
sillyBool = even $ product $ iterate [ 1 :: Integer, 2, 3]
sillyBoolCompiled = $$(compile [|| sillyBool ||])
sillyBoolFlat = flat sillyBoolCompiled

runMain fp = do
    withFile fp WriteMode $ \hdl -> B.hPutStr hdl sillyBoolFlat