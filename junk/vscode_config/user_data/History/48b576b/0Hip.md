## Incorrect validation of Proposal execution allows stealing of the Upgrade Token

| Severity | CVSS  | Vulnerability types |
| -- | -- | -- |
| Critical | [9.3](https://nvd.nist.gov/vuln-metrics/cvss/v3-calculator?vector=AV:N/AC:H/PR:N/UI:N/S:C/C:N/I:H/A:N/E:F/RL:O/RC:C/CR:X/IR:H/AR:M/MAV:N/MAC:L/MPR:N/MUI:N/MS:C/MC:X/MI:H/MA:X&version=3.1) | multiple-satisfaction |

## Description

A user can steal an upgrade token by executing two simmilar proposals in the same transaction.

A passed proposal results in an UTXO with an `Upgrade Token`. There are pairs of `Upgrade Tokens` whose validation logic allows for them to be executed simultaneously in a single transaction.
For example two `Text Proposals` or two `Modify Protocol Params` proposals for the same parameters.

The insufficient validation logic checks precisely for a single `Upgrade Token` to get burnt inside .
This allows for a transaction spending two `Upgrade Tokens`, burning one of them as required by the validator, but sending the remaining one to a wallet address of an attacker.

An actor in possesion of an `Upgrade Token` could execute arbitrary proposals.

## How to reproduce

To test for the vulnerability we use two successful `Text Proposals` and execute them in a single transaction.

A [test](https://github.com/IndigoProtocol/smart-contracts/blob/3ceec2538fba0266b75c5e7cadddeae1fb8fb4e6/tests/Spec/Governance/Benchmark.hs#L197) was added in the `IndigoProtocol/smart-contracts` test suite that demonstrates the attack. To reproduce it's sufficient to run the following steps:

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

## Recommendedation

TODO: Where is that validation? Which function? Which validator?
The validation of proposal execution should check for the unique update token in the inputs.

## References

TODO: Add ALL the missing references
1. [CVSSS 3.1 Qualitative Severity Rating Scale](https://www.first.org/cvss/v3.1/specification-document)
2. [MLabs vulnerability classification](https://www.notion.so/Vulnerability-Types-ad39253c84ce443a82b835d94d765ba2)
3. [Indigo Yellow Paper](https://indigoprotocol.io/wp-content/uploads/2022/01/yellowpaper.pdf)
4. [Audit - Proposal Exploit #466](https://github.com/IndigoProtocol/smart-contracts/pull/466)
5. [Proposal exploit test](https://github.com/IndigoProtocol/smart-contracts/blob/3ceec2538fba0266b75c5e7cadddeae1fb8fb4e6/tests/Spec/Governance/Benchmark.hs#L197)
