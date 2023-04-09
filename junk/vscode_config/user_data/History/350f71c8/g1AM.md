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

Fix listed issues.

## Links

1. [Severity Levels for Security Issues](https://www.atlassian.com/trust/security/security-severity-levels)
1. [#478 - Audit - Documentation enhancements](https://github.com/IndigoProtocol/smart-contracts/issues/478)
