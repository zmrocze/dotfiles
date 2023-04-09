
## Title
Off-by-one error in calculating vested amounts 

## Title
Two indy inputs, indy burned

## Title

Indy can't be minted. Otherwise assumptions of Vesting validator don't hold. 

## Vulns for types for Vesting validator

### other-redeemer
#### Description
Redeemer of the validated input is checked. No other validator gets involved. 
#### Conclusion
Vulnerability doesn't exist.

### other-token-names
#### Conclusion
Indy cannot get minted, so vulnerability doesn't apply.

### unbounded-protocol-datum
#### Conclusion
Covered by arbitrary-utxo-datum.

### X arbitrary-utxo-datum
#### Description
The output of a transaction unlocking vesting funds that goes to the distributor is not checked for any specific datum.
This allows creation of an unspendable output due to increased transaction execution units, effectively burning the INDY funds.
#### Conclusion
Vulnerability found.

### unbounded-protocol-value
#### Description
INDY tokens cannot get minted after the initial mint. Their constant amount in circulation is checked for by the vesting validator.
### Conclusion
No vulnerability.

### foreign-utxo-tokens
#### Description
The vesting validator checks for existance of UTXOs with precisely the expected INDY tokens and no other tokens.
#### Conclusion
No vulnerability.

### multiple-satisfaction
#### Description
The vesting validator ensures a single UTXO with INDY gets spent. 
#### Conclusion 
No vulnerability.

### multiple-mint-satisfaction
### Description
### Conclusion
Covered by multiple-satisfaction.

### locked-ada
#### Description
The vesting validator can be interacted with in a way that doesn't lock Ada in unspendable outputs.
#### Conclusion
No direct vulnerability, though .

### locked-nonada-values
#### Description
The vesting validator allows for up to two outputs with INDY tokens. It checks if together they hold exactly as much INDY as gets spent in the transaction.
#### Conclusion
No vulnerability.

### missing-utxo-authentication
#### Description
The protocol mantains a single INDY utxo at the vesting script address, given that the distributor is not the vesting script itself. The validator has to spend that and only that INDY input.
#### Conclusion
No vulnerability.

### missing-incentive
#### Description
There is incentive for anyone profiting from the vesting funds to unlock them with the vesting contract.
#### Conclusion
No vulnerability.

### bad-incentive
#### Description
#### Conclusion
Not found.

### utxo-contention
#### Description
The protocol uses a single global utxo, though vesting funds can be unlocked only once every 5 days, so contention is not any problem.
### Conclusion
No vulnerability.

### cheap-spam
#### Description
#### Conclusion
Not found.

### insufficient-tests
#### Description
#### Conclusion

### incorrect-documentation
#### Description
#### Conclusion
Not found.

### insufficient-staking-control
#### Description
The vesting validator expects two outputs. Distributor output is sent to an address. Continuing output address can't contain staking credentials. Both of the outputs have fixed staking credentials.
#### Conclusion
No vulnerability.

### incorrect-logic
#### Description
#### Conclusion
Not found.

### unchecked-datum
#### Description
#### Conclusion
Not found.
