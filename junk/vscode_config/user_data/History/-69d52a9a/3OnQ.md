# Lack of tests for staking rewards

| Severity | CVSS | Vulnerability type |
| -------- | ---- | ------------------ |
| None     | N/A  | missing-tests      |

## Description

There are no tests that attempt to run the protocol with a non-zero staking reward. A lack of tests means the behaviour of the code is not fully determined and cannot be relied upon. The logic behind reward calculation is never tested, and the following sequences of transactions are untested:

- [Distribute from collector to staking manager](https://github.com/IndigoProtocol/smart-contracts/blob/532d8cb96e81955a812d823417b742c4f6415f4a/src/Indigo/Contracts/Staking/OnChain.hs#L170 "distribute validator") and [unstake a staking position](https://github.com/IndigoProtocol/smart-contracts/blob/532d8cb96e81955a812d823417b742c4f6415f4a/src/Indigo/Contracts/Staking/OnChain.hs#L322 "unstake validator")

- [Distribute from collector to staking manager](https://github.com/IndigoProtocol/smart-contracts/blob/532d8cb96e81955a812d823417b742c4f6415f4a/src/Indigo/Contracts/Staking/OnChain.hs#L170 "distribute validator") and [adjust a staking position](https://github.com/IndigoProtocol/smart-contracts/blob/532d8cb96e81955a812d823417b742c4f6415f4a/src/Indigo/Contracts/Staking/OnChain.hs#L240 "adjust validator")

- Performing the above with multiple active staking positions

## Recommendation

Add relevant tests to increase the confidence that the implementation indeed works as specified. [A PR](https://github.com/IndigoProtocol/smart-contracts/pull/480) was introduced during the audit that adds the missing test cases, however the changes introduced weren't considered during the audit.

## References

1. [Protocol Fee Tests #480](https://github.com/IndigoProtocol/smart-contracts/pull/480)

2. [Audit - No staking reward tests #469](https://github.com/IndigoProtocol/smart-contracts/pull/469)

3. Relevant validators:
   1. [`validateDistribute`](https://github.com/IndigoProtocol/smart-contracts/blob/532d8cb96e81955a812d823417b742c4f6415f4a/src/Indigo/Contracts/Staking/OnChain.hs#L170 "distribute validator")
   2. [`validateUnstake`](https://github.com/IndigoProtocol/smart-contracts/blob/532d8cb96e81955a812d823417b742c4f6415f4a/src/Indigo/Contracts/Staking/OnChain.hs#L322 "unstake validator")
   3. [`validateAdjustStakedAmount`](https://github.com/IndigoProtocol/smart-contracts/blob/532d8cb96e81955a812d823417b742c4f6415f4a/src/Indigo/Contracts/Staking/OnChain.hs#L240 "adjust validator")
