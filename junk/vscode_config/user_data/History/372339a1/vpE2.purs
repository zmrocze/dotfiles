module Internal.Plutus.Types.ScriptContext where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)

import Internal.Plutus.Types.ScriptContext.V1.TxInfo as V1
import Internal.Plutus.Types.ScriptContext.V2.TxInfo as V2

data TxInfo
  = V1 V1.TxInfoV1
  | V2 V2.TxInfoV2

derive instance Eq TxInfo
derive instance Generic TxInfo _

instance Show TxInfo where 
  show = genericShow

-- Transaction context shown to validators.
newtype ScriptContext  = ScriptContext {
  scriptContextTxInfo :: TxInfo,
  scriptContextPurpose :: V2.ScriptPurpose 
}

derive instance Eq ScriptContext
derive instance Newtype ScriptContext _
derive instance Generic ScriptContext _

instance Show ScriptContext where 
  show = genericShow

instance ToData ScriptContext where 
  toData = genericToData