
There are few inconsistencies in the `Tx` type and its relation to `ScriptLookups` and `TxStripped`. Possibly all of them stem only from my misunderstanding. `Tx` lacks support for withdrawals or certifications which I'd like to add, but probably it is better to first address these.

### Should datums be stored on the side in `Map DatumHash Datum` field or instead packed together with inputs?

`TxIn` for now CAN contain datum as:
```
data TxInType =
      -- TODO: these should all be hashes, with the validators and data segregated to the side
      ConsumeScriptAddress !Validator !Redeemer !Datum -- ^ A transaction input that consumes a script address with the given validator, redeemer, and datum.
    | ConsumePublicKeyAddress -- ^ A transaction input that consumes a public key address.
    | ConsumeSimpleScriptAddress -- ^ Consume a simple script

-- | A transaction input, consisting of a transaction output reference and an input type.
data TxIn = TxIn {
    txInRef  :: !TxOutRef,
    txInType :: Maybe TxInType
    }
```
The TODO comment is not mine. 
But `Transaction` also has the `txData` field for various datums (which ones exactly if datums can be stored with TxIn?). 
So which is better: 
a) datum in TxIn or 
b) datum in a map and referenced by hash in TxIn'?

Also could someone explain at what stage is can the `txInType` field be `Nothing` and when is it expected to be `Just ...`? For example `toCardanoTxBody` fails if it's `Nothing`.

### Should validators be stored on the side in seperate container or instead packed together with inputs/withdrawals/etc?

This is simmilar to the previous one but here validator scripts apply not only to inputs but also minting, certyfying and withdrawing.

Currently input validators are stored alongside inputs ^, minting policies are stored in a set and withdrawals or delegation certificates are not yet accounted for.

Should we:
a) keep input validators with inputs, certyficate validators with certyficates, withdrawal validator with withdrawals, minting validators in a set
b) input validators in one set, certyficate validators in second set, ..., minting policies in 4th set
c) all scripts in a single set
d) other combination of above solutions

Here b) is wasteful as single script can be used for both minting and withdrawing (certyfying also?). a) keeps the most information. c) splits between what transactions contain in ledger and what they contain when are submitted for verification. 
