
module Ledger.Tx.Types.Certificate 

data Certificate = Certificate
  { certificateDcert    :: DCert
  , certificateRedeemer :: Maybe Redeemer           -- ^ redeemer for script credential
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Serialise, NFData)

instance Pretty Certificate where
    pretty = viaShow
