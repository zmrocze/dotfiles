# Missing check in the Poll validator to ensure that inputs from other scripts are spent with the correct redeemer is unsafe

| Severity | CVSS | Vulnerability type |
|---|---|---|
| Medium | [5.7](https://nvd.nist.gov/vuln-metrics/cvss/v3-calculator?vector=AV:N/AC:L/PR:N/UI:N/S:U/C:N/I:L/A:L/E:U/RL:O/RC:X&version=3.1) | other-redeemer |

## Description

The *Other Redeemer* vulnerability was identified in the Poll validator and specifically the `Indigo.Contracts.Governance.Poll.OnChain.validateVote` function that validates spending from the Poll validator with the Poll's `Vote` redeemer.
The function implicitly relies that the transaction spends an input from the Staking validator with the Staking's `Lock` redeemer, but performs no explicit checks to ensure it, which is considered an unsafe practice.

In the future, if the Poll validator is modified it could inadvertently introduce a vulnerability.

In the pre audit engagement, a vulnerability of this type was indeed [found and reported](https://github.com/IndigoProtocol/smart-contracts/issues/416). 

## How to reproduce

No exploit for the vulnerability was identified during the audit.

However, hypothetically speaking, if a Staking validator redeemer was available for use such that the vulnerable `Indigo.Contracts.Governance.Poll.OnChain.validateVote` still successfully validated, the vulnerability could become exploitable as some checks in `Indigo.Contracts.Staking.OnChain.validateLock` would not be performed.

## Recommendation

Add an explicit check in the `Indigo.Contracts.Governance.Poll.OnChain.validateVote` function using the `Indigo.Utils.Helpers.usesSpendRedeemer` function to assert that the transaction spends an input from the Staking validator using the correct Staking's `Lock` redeemer.

## References

1. [CVSSS 3.1 Qualitative Severity Rating Scale](https://www.first.org/cvss/v3.1/specification-document)
2. [MLabs vulnerability classification](https://www.notion.so/Vulnerability-Types-ad39253c84ce443a82b835d94d765ba2)
3. [Indigo Yellow Paper](https://indigoprotocol.io/wp-content/uploads/2022/01/yellowpaper.pdf)
4. [Poll validator](https://github.com/IndigoProtocol/smart-contracts/blob/audit/main/src/Indigo/Contracts/Governance/Poll/OnChain.hs#L51)
5. [Poll validateVote](https://github.com/IndigoProtocol/smart-contracts/blob/audit/main/src/Indigo/Contracts/Governance/Poll/OnChain.hs#L51)
6. [Staking validator](https://github.com/IndigoProtocol/smart-contracts/blob/audit/main/src/Indigo/Contracts/Staking/OnChain.hs#L35)
7. [Staking validateLock](https://github.com/IndigoProtocol/smart-contracts/blob/audit/main/src/Indigo/Contracts/Staking/OnChain.hs#L382)
