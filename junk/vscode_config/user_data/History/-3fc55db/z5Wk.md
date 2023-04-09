# Insufficient testing can impact correctness

| Severity | CVSS | Vulnerability types |
| -- | -- | -- |
| None | [4.5](https://nvd.nist.gov/vuln-metrics/cvss/v3-calculator?vector=AV:N/AC:H/PR:N/UI:N/S:C/C:N/I:L/A:N/E:U/RL:X/RC:X/CR:H/IR:H/AR:M/MAV:X/MAC:X/MPR:X/MUI:X/MS:X/MC:N/MI:L/MA:N&version=3.1) | insufficient-tests |

## Description

There are no tests asserting the correctness of spending collected protocol fees.
Protocol fees get sent to the [`Collector`](https://github.com/IndigoProtocol/smart-contracts/blob/c2748d1c03d089fcf913d31ace378a4920e909bd/src/Indigo/Contracts/Collector/OnChain.hs#L30) script to then get distributed to Indy stakers. Distributing is not tested in the current test suite. Thus the validation logic of the [`Collector`](https://github.com/IndigoProtocol/smart-contracts/blob/c2748d1c03d089fcf913d31ace378a4920e909bd/src/Indigo/Contracts/Collector/OnChain.hs#L30) script with the `Collect` redeemer and the one of [`Staking Manager`](https://github.com/IndigoProtocol/smart-contracts/blob/c2748d1c03d089fcf913d31ace378a4920e909bd/src/Indigo/Contracts/Staking/OnChain.hs#L49) with the `Distribute` redeemer doesn't get executed durings tests.

## Recommendation

Add tests attempting to steal the collected fees, spending `Collector` owned UTXOs to a private address. Add tests for the distribution of collected fees to Indy stakers, both where the `Staking` validator succeeds and ones where it doesn't due to unfair rewards distribution or unautharized spend to a private address.

## References

1. [CVSSS 3.1 Qualitative Severity Rating Scale](https://www.first.org/cvss/v3.1/specification-document)
2. [MLabs vulnerability classification](https://www.notion.so/Vulnerability-Types-ad39253c84ce443a82b835d94d765ba2)
3. [Indigo Yellow Paper](https://indigoprotocol.io/wp-content/uploads/2022/01/yellowpaper.pdf)
4. [Audit - No Collector fee tests #446](https://github.com/IndigoProtocol/smart-contracts/pull/446)
5. [Collector validator](https://github.com/IndigoProtocol/smart-contracts/blob/c2748d1c03d089fcf913d31ace378a4920e909bd/src/Indigo/Contracts/Collector/OnChain.hs#L30)
6. [Staking validator](https://github.com/IndigoProtocol/smart-contracts/blob/c2748d1c03d089fcf913d31ace378a4920e909bd/src/Indigo/Contracts/Staking/OnChain.hs#L49)