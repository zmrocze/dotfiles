In plutus scripts addresses are used in their most fundamental encoding: as bytestrings resulting from hashing (PubKeyHash, ScriptHash).
Remainder that plutus `Address` contains a `Credential` and maybe a `StakingCredential`, where `StakingCredential` is also just a `Credential` or a pointer to the certificate.
A credential is: 
```
data Credential
  =
    PubKeyCredential PubKeyHash
  | ScriptCredential ScriptHash
```
and as hashes can be any bytestrings the only possible validation on the plutus side is the length validation - both of these should be 28 byte.

Though I expect the biggest painpoint to be obtaining hashes for users using wallets or cli.

### Key hash from shelley address

The usual Shelley address looks like this:
```
addr1v9hynzag0uyah44hgnfxfd9nawx5yvnfa0yd26ncnwtysnsz3lc3u
``` 
This is bech32 encoded and in this form contains additional metadata outside of the hash, namely the address type and the network.
Use `cardano-cli address build` to build the address.

We can run 
```
cardano-cli address info --address addr1v9hynzag0uyah44hgnfxfd9nawx5yvnfa0yd26ncnwtysnsz3lc3u
```
getting back
```
{
    "type": "payment",
    "base16": "616e498ba87f09dbd6b744d264b4b3eb8d423269ebc8d56a789b96484e",
    "era": "shelley",
    "encoding": "bech32",
    "address": "addr1v9hynzag0uyah44hgnfxfd9nawx5yvnfa0yd26ncnwtysnsz3lc3u"
}
```
The base16 field is of interest to us. It is 58 characters long, so it encodes 29 bytes long bytestring.
That's right as the first byte is a header with above mentioned metadata. Remaining 28 bytes is the hash.
Result: 
 - the hex encoded pubkey/validator hash is `6e498ba87f09dbd6b744d264b4b3eb8d423269ebc8d56a789b96484e`
 - can get raw bytestring with `xxd -r -p <<< 616e498ba87f09dbd6b744d264b4b3eb8d423269ebc8d56a789b96484e`

### Key hash from verification key file

Use
```
cardano-cli address key-hash --payment-verification-key-file <<vkey filepath>>
```
getting back the hex encoded key hash.

### Minting policy hash

Use 
```
cardano-cli transaction policyid --script-file <<script filepath>>
```


### Getting hashes in haskell code.

Two options:

  a) Save the raw bytestring to a file and read it directly from code.

  b) Use `IsString` instance of `PubKeyHash`/`ScriptHash` that uses hex encoding:

```
pubkey :: PubKeyHash
pubkey = fromString "6e498ba87f09dbd6b744d264b4b3eb8d423269ebc8d56a789b96484e"
```

Use b) when asking for user input. Verifying the length of resulting bytestring can rule out common cases of misunderstanding.
