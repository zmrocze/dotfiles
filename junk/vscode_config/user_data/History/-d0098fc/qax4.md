# Incorrect validation of _CDP_ liquidation transactions allows burning arbitrary amount of iAsset from the Stability Pool

| Severity | CVSS | Vulnerability type |
| --- | --- | --- |
| Critical | [9.5](https://nvd.nist.gov/vuln-metrics/cvss/v3-calculator?vector=AV:N/AC:L/PR:N/UI:N/S:C/C:N/I:H/A:N/E:H/RL:O/RC:C/CR:X/IR:H/AR:M/MAV:N/MAC:L/MPR:N/MUI:N/MS:C/MC:X/MI:H/MA:X&version=3.1) | incorrect-logic |

## Description

Incorrect validation of _CDP_ liquidation transactions allows anyone to burn arbitrary amount of iAsset from the Stability Pool.

Frozen CDPs are eligible for "liquidation" by a Stability Pool associated with the same iAsset. _CDP_ liquidation transactions come in two forms:

1. Full liquidation transaction that liquidates all of the minted iAssets associated with a CDP,
2. Partial liquidation transaction that liquidates only a part of the minted iAssets associated with a CDP.

An incorrect validation of full liquidation transactions was identified in the function

```
Indigo.Contracts.CDP.OnChain.validateLiquidateCDP
```

allowing anyone to burn arbitrary amount of iAsset from the Stability Pool regardless of the amount of iAsset actually minted by the _CDP_ being liquidated.

For example, If a frozen _CDP_ has 1 minted _iAsset_, and the Stability Pool has 1,000,000 iAsset staked. Then a full liquidation transaction can be submitted that burns all $1,000,000$ _iAsset_ from the Stability Pool.

## How To Reproduce

To test for the vulnerability it is sufficient to build and submit a full _CDP_ liquidation transaction that burns more _iAssets_ than are minted by the frozen CDP.

A `Liquidation exploit test` was added in the IndigoProtocol/smart-contracts test suite that demonstrates the attack. To reproduce it is sufficient to run the following steps:

```bash
$ git clone https://github.com/IndigoProtocol/smart-contracts.git
$ cd smart-contracts
$ git switch audit/liquidation-exploit
$ nix develop
$ cabal run -f-plutonomy indigo-tests -- --test -- -p "/liquidate exploit succeeds/"
Up to date
benchmark test suite
  CDP
    liquidate exploit succeeds: OK (0.39s)

All 1 tests passed (0.39s)
```

## Recommendation

The validation of full liquidation transactions should include a check to ensure that the burned iAsset amount doesn't exceed the amount of iAsset minted by the _CDP_ being liquidated.

<!-- TODO: Add a note on the fix provided during the audit? -->

## References

1. [CVSS 3.1 Qualitative Severity Rating Scale](https://www.first.org/cvss/v3.1/specification-document)
2. [MLabs vulnerability classification](https://www.notion.so/Vulnerability-Types-ad39253c84ce443a82b835d94d765ba2)
3. [Indigo Yellow Paper](https://indigoprotocol.io/wp-content/uploads/2022/01/yellowpaper.pdf)
4. [Audit - Liquidity Pool Liquidation Exploit #441](https://github.com/IndigoProtocol/smart-contracts/pull/441)
5. [Liquidation exploit test](https://github.com/IndigoProtocol/smart-contracts/blob/audit/liquidation-exploit/tests/Spec/CDP/Benchmark.hs#L247)
6. [CDP validator](https://github.com/IndigoProtocol/smart-contracts/blob/audit/main/src/Indigo/Contracts/CDP/OnChain.hs#L657)
7. [CDP validateLiquidateCDP](https://github.com/IndigoProtocol/smart-contracts/blob/audit/main/src/Indigo/Contracts/CDP/OnChain.hs#L318)
