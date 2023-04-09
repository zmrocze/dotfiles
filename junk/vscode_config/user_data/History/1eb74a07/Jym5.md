#### 2.3.2 Incorrect validation of Proposal execution allows stealing of the Upgrade Token.

| Severity | CVSS | Vulnerability type |
|---|---|---|
| <span style="color:red">TODO</span> | [9.3](TODO) | multiple-satisfaction |

##### Description

If there are two successful proposals, then a user can steal an upgrade token by executing both of the proposals in the same transaction. 

A passed proposal results in an UTXO with an `Upgrade Token`. There are pairs of `Upgrade Tokens` whose validation logic allows for them to be executed simultaneously in a single transaction. For example two `Text Proposals` or two `Modify Protocol Params` proposals for the same parameters.

Though the validation logic checks precisely for a single `Upgrade Token` to get burnt. This allows for a transaction spending two `Upgrade Tokens`, burning one of them as demanded by the validator, but sending the remaining one to a wallet address of an attacker. This is very severe as a user having an upgrade token could execute arbitrary proposals.

##### How To Reproduce

To test for the vulnerability we use two successful `Text Proposals` and execute them in a single transaction. 

A [test](https://github.com/IndigoProtocol/smart-contracts/blob/3ceec2538fba0266b75c5e7cadddeae1fb8fb4e6/tests/Spec/Governance/Benchmark.hs#L197) was added in the IndigoProtocol/smart-contracts test suite that demonstrates the attack. To reproduce it's sufficient to run the following steps:

```bash
$ git clone https://github.com/IndigoProtocol/smart-contracts.git
$ cd smart-contracts
$ git checkout 3ceec2538fba0266b75c5e7cadddeae1fb8fb4e6
$ nix develop
$ cabal run indigo-tests -- --test -- --pattern "Text Exploit Proposal"
benchmark test suite
  Governance
    Text Exploit Proposal: OK (0.65s)
All 1 tests passed (0.65s)
```

##### Recommendation

The validation of proposal execution should check for the unique update token in the inputs. Ability to execute multiple proposals at once seems more confusing than usefull.

##### References

1. [Severity Levels for Security Issues](https://www.atlassian.com/trust/security/security-severity-levels)
2. [MLabs vulnerability classification](https://www.notion.so/Vulnerability-Types-ad39253c84ce443a82b835d94d765ba2)
3. [Audit - Proposal Exploit #466](https://github.com/IndigoProtocol/smart-contracts/pull/466)
4. [Liquidation exploit test](https://github.com/IndigoProtocol/smart-contracts/blob/3ceec2538fba0266b75c5e7cadddeae1fb8fb4e6/tests/Spec/Governance/Benchmark.hs#L197)