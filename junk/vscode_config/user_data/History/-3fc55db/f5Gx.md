# Insufficient testing can impact correctness

| Severity | CVSS | Vulnerability types |
| -- | -- | -- |
| Low | [3.9](https://nvd.nist.gov/vuln-metrics/cvss/v3-calculator?vector=AV:N/AC:H/PR:N/UI:N/S:C/C:N/I:L/A:N/E:U/RL:O/RC:U/CR:H/IR:H/AR:M/MAV:X/MAC:X/MPR:X/MUI:X/MS:X/MC:N/MI:L/MA:N&version=3.1) | insufficient-tests |

## Description

There are no tests asserting the correctness of spending collected protocol fees. Protocol fees get sent to the [`Collector`](https://github.com/IndigoProtocol/smart-contracts/blob/c2748d1c03d089fcf913d31ace378a4920e909bd/src/Indigo/Contracts/Collector/OnChain.hs#L30) script to then get distributed to the Indy stakers. Reward collection involves the [`Collector`](https://github.com/IndigoProtocol/smart-contracts/blob/c2748d1c03d089fcf913d31ace378a4920e909bd/src/Indigo/Contracts/Collector/OnChain.hs#L30) and [`Staking`](https://github.com/IndigoProtocol/smart-contracts/blob/c2748d1c03d089fcf913d31ace378a4920e909bd/src/Indigo/Contracts/Staking/OnChain.hs#L49) validators. A lack of tests means the behaviour of the code is not fully determined and cannot be relied upon. The logic behind reward calculation is never tested, and the following sequences of transactions are untested:

- [Distribute from collector to staking manager](https://github.com/IndigoProtocol/smart-contracts/blob/532d8cb96e81955a812d823417b742c4f6415f4a/src/Indigo/Contracts/Staking/OnChain.hs#L170 "distribute validator") and [unstake a staking position](https://github.com/IndigoProtocol/smart-contracts/blob/532d8cb96e81955a812d823417b742c4f6415f4a/src/Indigo/Contracts/Staking/OnChain.hs#L322 "unstake validator")

- [Distribute from collector to staking manager](https://github.com/IndigoProtocol/smart-contracts/blob/532d8cb96e81955a812d823417b742c4f6415f4a/src/Indigo/Contracts/Staking/OnChain.hs#L170 "distribute validator") and [adjust a staking position](https://github.com/IndigoProtocol/smart-contracts/blob/532d8cb96e81955a812d823417b742c4f6415f4a/src/Indigo/Contracts/Staking/OnChain.hs#L240 "adjust validator")

- Performing the above with multiple active staking positions

## Recommendation

Add relevant tests to increase the confidence that the implementation indeed works as specified. should attempt to steal the collected fees, spending `Collector` owned UTXOs to a private address. Tests should cover the scenarios above, both succeeding and failing.  and verify the calculations for the distribution of collected fees to Indy stakers, both where the `Staking` validator succeeds and ones where it doesn't due to unfair rewards distribution or unautharized spend to a private address.

## References

1. [CVSSS 3.1 Qualitative Severity Rating Scale](https://www.first.org/cvss/v3.1/specification-document)
2. [MLabs vulnerability classification](https://www.notion.so/Vulnerability-Types-ad39253c84ce443a82b835d94d765ba2)
3. [Indigo Yellow Paper](https://indigoprotocol.io/wp-content/uploads/2022/01/yellowpaper.pdf)
4. [Audit - No Collector fee tests #446](https://github.com/IndigoProtocol/smart-contracts/pull/446)
5. [Collector validator](https://github.com/IndigoProtocol/smart-contracts/blob/c2748d1c03d089fcf913d31ace378a4920e909bd/src/Indigo/Contracts/Collector/OnChain.hs#L30)
6. [Staking validator](https://github.com/IndigoProtocol/smart-contracts/blob/c2748d1c03d089fcf913d31ace378a4920e909bd/src/Indigo/Contracts/Staking/OnChain.hs#L49)
