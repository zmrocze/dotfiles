
# Changes

### Redefines `Tx` type
 - `Tx` gets withdrawals, certificates(1), metadata(1) fields
 - script and datum witnesses get put into maps and refered to by hashes
 - redeemers get put together with the MintingPolicy/Certificate/Input
 - defines `TxInput` and `TxInputType` simmilar to `TxIn` and `TxInType`.
 - defines `Withdrawal` type
 - defines `Certificate` type
 - splits Ledger.Tx.Internal module

### Updates pab accordingly
 - `TxIn` remains used by some functions
 - most changes are automatic: imports and minor updates that don't change logic but update the `Tx`
 - `toCardanoTxBodyContent` from Ledger.Tx.CardanoAPI gets updated (1) 

### Updates purescript accordingly
 - code generation generates added types: `Certificate`, `Withdrawal`, `TxInput`, `TxIn`
 - minor tweak in purescript source code



# Certificate and Metadata is chosen wrong (1)


  using `DCert`. Only `DCert` is incorrect for that as some info is digested, better would `Cardano.Api.Certificate`.