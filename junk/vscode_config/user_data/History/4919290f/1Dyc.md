
- [ ] other-redeemer
Redeemer of the validated input is checked. No other validator gets involved. 
Vulnerability doesn't exist.
- [ ] other-token-names
Indy cannot get minted, so vulnerability doesn't apply.
- [ ] unbounded-protocol-datum
Covered by arbitrary-utxo-datum.
- [ ] arbitrary-utxo-datum
The output of a transaction unlocking vesting funds that goes to the distributor is not checked for any specific datum.
This allows creation of an unspendable output due to increased transaction execution units, effectively burning the INDY funds.
Vulnerability found.
- [ ] unbounded-protocol-value
INDY tokens cannot get minted after the initial mint. Their constant amount in circulation is checked for by the vesting validator.
No vulnerability.
- [ ] foreign-utxo-tokens
The vesting validator checks for existance of UTXOs with precisely the expected INDY tokens and no other tokens.
No vulnerability.
- [ ] multiple-satisfaction
The vesting validator ensures only a single UTXO with INDY gets spent.  
No vulnerability.
- [ ] multiple-mint-satisfaction
Covered by multiple-satisfaction.
- [ ] locked-ada
The protocol doesn't require creation of any unspendable Ada outputs.
No vulnerability.
- [ ] locked-nonada-values
The vesting validator allows for up to two outputs with INDY tokens. It checks if together they hold exactly as much INDY as gets spent in the transaction.
No vulnerability.
- [ ] missing-utxo-authentication
The protocol mantains a single INDY utxo at the vesting script address, given that the distributor is not the vesting script itself. The validator has to spend that and only that INDY input.
No vulnerability.
- [ ] missing-incentive
There is incentive for anyone profiting from the vesting funds to unlock them with the vesting contract.
No vulnerability.
- [ ] bad-incentive
Not found.
- [ ] utxo-contention
The protocol uses a single global utxo, though vesting funds can be unlocked only once every 5 days, so contention is not anyproblem.
- [ ] Conclusion
No vulnerability.
- [ ] cheap-spam
Not found.
- [ ] insufficient-tests
- [ ] incorrect-documentation
Not found.
- [ ] insufficient-staking-control
The vesting validator expects two outputs. Distributor output is sent to an address. Continuing output address can't contain staking credentials. Both of the outputs have fixed staking credentials.
No vulnerability.
- [ ] incorrect-logic
Not found.
- [ ] unchecked-datum
Not found.
- [ ] infinite-minting
Vesting validator is not a minting policy.
No vulnerability.
- [ ] division-by-zero
Every division in the vesting validator is by a positive constant.
No vulnerability.
- [ ] rounding-error
Not found in the locked/unlocked funds calculations.
- [ ] sybil-contention
Doesn't apply to the vesting validator.
Not found.
- [ ] poor-code-standards
Not found.