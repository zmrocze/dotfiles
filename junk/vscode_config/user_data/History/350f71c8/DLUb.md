## Documentation enhancements

| Severity | CVSS RATING | vulnerability types |
| -- | -- | -- |
| None | [0.0](https://nvd.nist.gov/vuln-metrics/cvss/v3-calculator?vector=AV:N/AC:L/PR:N/UI:N/S:C/C:N/I:N/A:N/E:U/RL:X/RC:X/CR:H/IR:H/AR:M/MAV:X/MAC:X/MPR:X/MUI:X/MS:X/MC:N/MI:N/MA:N&version=3.1) | insufficient-documentation incorrect-documentation |

## Description 
Proper documentation is crucial to avoid introducing new bugs in the future.
Codebase generally contains few comments, but the consistent naming and module structure together with self-commenting validator checks make it a lesser problem.

Following are issues that could be improved still:

1. `RecordEpochToScaleToSum` endpoint is missing from the yellow-paper.
1. Minimal collateral ratio is passed in code as a percentage. This is undocumented both in the types and functions using it.
2. `sessSnapshot` field is confusingly named. Elsewhere "snapshot" refers to values with `StabilityPoolSnapshot` type.
3. The implementation of the tests is poorly documented.

## How To Reproduce

N/A

## Recommended Fix

We recommend fixing confusing.

## References

1. [CVSSS 3.1 Qualitative Severity Rating Scale](https://www.first.org/cvss/v3.1/specification-document)
2. [MLabs vulnerability classification](https://www.notion.so/Vulnerability-Types-ad39253c84ce443a82b835d94d765ba2)
3. [Indigo Yellow Paper](https://indigoprotocol.io/wp-content/uploads/2022/01/yellowpaper.pdf)
4. [#478 - Audit - Documentation enhancements](https://github.com/IndigoProtocol/smart-contracts/issues/478)