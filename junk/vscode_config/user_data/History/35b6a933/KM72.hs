{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

module Plutus.V1.Ledger.Crypto(
    PubKeyHash(..)
    ) where

import Control.DeepSeq (NFData)
import Data.String
import GHC.Generics (Generic)
import Plutus.V1.Ledger.Bytes (LedgerBytes (..))
import PlutusTx qualified
import PlutusTx.Lift (makeLift)
import PlutusTx.Prelude qualified as PlutusTx
import Prettyprinter
import qualified Data.ByteString as BS

-- | The hash of a public key. This is frequently used to identify the public key, rather than the key itself.
-- Should be 28 bytes.
newtype PubKeyHash = UnsafePubKeyHash { getPubKeyHash :: PlutusTx.BuiltinByteString }
    deriving stock (Eq, Ord, Generic)
    deriving anyclass (NFData)
    deriving newtype (PlutusTx.Eq, PlutusTx.Ord, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
    deriving IsString via LedgerBytes
    deriving (Show, Pretty) via LedgerBytes
makeLift ''PubKeyHash

pubKeyHash bs = 
    if BS.length bs == len then
        Nothing
    else
        Just $ f $ toBuiltin bs
