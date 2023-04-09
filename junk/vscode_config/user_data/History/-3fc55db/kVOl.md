# Insufficient tests for reward collection and distribution

| Severity | CVSS | Vulnerability types |
| -- | -- | -- |
| Low | [3.9](https://nvd.nist.gov/vuln-metrics/cvss/v3-calculator?vector=AV:N/AC:H/PR:N/UI:N/S:C/C:N/I:L/A:N/E:U/RL:O/RC:U/CR:H/IR:H/AR:M/MAV:X/MAC:X/MPR:X/MUI:X/MS:X/MC:N/MI:L/MA:N&version=3.1) | insufficient-tests |

## Description

There are no tests asserting the correctness of collecting and distributing protocol fees. Protocol fees get sent to the [`Collector`](https://github.com/IndigoProtocol/smart-contracts/blob/c2748d1c03d089fcf913d31ace378a4920e909bd/src/Indigo/Contracts/Collector/OnChain.hs#L30) script to then get distributed to the Indy stakers. Reward collection involves the [`Collector`](https://github.com/IndigoProtocol/smart-contracts/blob/c2748d1c03d089fcf913d31ace378a4920e909bd/src/Indigo/Contracts/Collector/OnChain.hs#L30) and [`Staking`](https://github.com/IndigoProtocol/smart-contracts/blob/c2748d1c03d089fcf913d31ace378a4920e909bd/src/Indigo/Contracts/Staking/OnChain.hs#L49) validators. A lack of tests means the behaviour of the code is not fully determined and cannot be relied upon. The logic behind reward calculation is never tested, and the following sequences of transactions are untested:

- [Distribute from collector to staking manager](https://github.com/IndigoProtocol/smart-contracts/blob/532d8cb96e81955a812d823417b742c4f6415f4a/src/Indigo/Contracts/Staking/OnChain.hs#L170 "distribute validator") and [unstake a staking position](https://github.com/IndigoProtocol/smart-contracts/blob/532d8cb96e81955a812d823417b742c4f6415f4a/src/Indigo/Contracts/Staking/OnChain.hs#L322 "unstake validator")

- [Distribute from collector to staking manager](https://github.com/IndigoProtocol/smart-contracts/blob/532d8cb96e81955a812d823417b742c4f6415f4a/src/Indigo/Contracts/Staking/OnChain.hs#L170 "distribute validator") and [adjust a staking position](https://github.com/IndigoProtocol/smart-contracts/blob/532d8cb96e81955a812d823417b742c4f6415f4a/src/Indigo/Contracts/Staking/OnChain.hs#L240 "adjust validator")

- Performing the above with multiple active staking positions

- Adding protocol fees to an existing collector through [this validation path](https://github.com/IndigoProtocol/smart-contracts/blob/38f2a52b99e4ccb976d6a18e985927062eae6d8a/src/Indigo/Contracts/Collector/OnChain.hs#L50-L52)

## Recommendation

Add relevant tests to increase the confidence that the implementation indeed works as specified. Tests should cover the scenarios above, covering the positive case and also expectedly failing for incorrect reward calculation or unauthorized spend attempts. Tests should attempt to steal the collected fees, spending `Collector` owned UTXOs to a private address.

Two PRs were opened which attempt to address these issues, however they were not considered as part of the audit:

1. [Collector Test Suite #479](https://github.com/IndigoProtocol/smart-contracts/pull/479)
1. [Protocol Fee Tests #480](https://github.com/IndigoProtocol/smart-contracts/pull/480)

## References

1. [CVSSS 3.1 Qualitative Severity Rating Scale](https://www.first.org/cvss/v3.1/specification-document)
2. [MLabs vulnerability classification](https://www.notion.so/Vulnerability-Types-ad39253c84ce443a82b835d94d765ba2)
3. [Indigo Yellow Paper](https://indigoprotocol.io/wp-content/uploads/2022/01/yellowpaper.pdf)
4. [Audit - No Collector fee tests #446](https://github.com/IndigoProtocol/smart-contracts/pull/446)
5. [Audit - No staking reward tests #469](https://github.com/IndigoProtocol/smart-contracts/pull/469)
6. [Collector Test Suite #479](https://github.com/IndigoProtocol/smart-contracts/pull/479)
7. [Protocol Fee Tests #480](https://github.com/IndigoProtocol/smart-contracts/pull/480)
8. [Staking validator](https://github.com/IndigoProtocol/smart-contracts/blob/c2748d1c03d089fcf913d31ace378a4920e909bd/src/Indigo/Contracts/Staking/OnChain.hs#L49)
9. Relevant validator parts:
   1. [`validateDistribute`](https://github.com/IndigoProtocol/smart-contracts/blob/532d8cb96e81955a812d823417b742c4f6415f4a/src/Indigo/Contracts/Staking/OnChain.hs#L170 "distribute validator")
   2. [`validateUnstake`](https://github.com/IndigoProtocol/smart-contracts/blob/532d8cb96e81955a812d823417b742c4f6415f4a/src/Indigo/Contracts/Staking/OnChain.hs#L322 "unstake validator")
   3. [`validateAdjustStakedAmount`](https://github.com/IndigoProtocol/smart-contracts/blob/532d8cb96e81955a812d823417b742c4f6415f4a/src/Indigo/Contracts/Staking/OnChain.hs#L240 "adjust validator")
   4. [`validateCollectorScript`](https://github.com/IndigoProtocol/smart-contracts/blob/c2748d1c03d089fcf913d31ace378a4920e909bd/src/Indigo/Contracts/Collector/OnChain.hs#L30)
