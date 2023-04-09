# Incorrect validation of poll shard merging allows stealing of poll tokens

| Severity | CVSS | Vulnerability type |
| --- | --- | --- |
| Critical | [9.5](https://nvd.nist.gov/vuln-metrics/cvss/v3-calculator?vector=AV:N/AC:L/PR:N/UI:N/S:C/C:N/I:H/A:H/E:H/RL:O/RC:C/CR:X/IR:H/AR:H/MAV:N/MAC:L/MPR:N/MUI:N/MS:C/MC:N/MI:H/MA:H&version=3.1) | incorrect-logic |

## Description

Incorrect validation of poll shard merging allows anyone to steal all poll tokens and _Ada_ from a poll's shards, while invalidating the poll. This results in a total compromise of the protocol.

Each poll creates "poll shards" (validated by [`validatePoll`](https://github.com/IndigoProtocol/smart-contracts/blob/532d8cb96e81955a812d823417b742c4f6415f4a/src/Indigo/Contracts/Governance/Poll/OnChain.hs#L58 "validatePoll validator")) which tally votes, and hold authentication tokens known as "[poll tokens](https://github.com/IndigoProtocol/smart-contracts/blob/532d8cb96e81955a812d823417b742c4f6415f4a/tests/Utils/Mock.hs#L89 "poll token asset class")". When the voting period for a poll concludes, the poll shards are "merged" (validated by [`validateMergeShards`](https://github.com/IndigoProtocol/smart-contracts/blob/532d8cb96e81955a812d823417b742c4f6415f4a/src/Indigo/Contracts/Governance/Poll/OnChain.hs#L175 "poll token validateMergeShards") for poll tokens, and [`validateMergeShardsManager`](https://github.com/IndigoProtocol/smart-contracts/blob/532d8cb96e81955a812d823417b742c4f6415f4a/src/Indigo/Contracts/Governance/Poll/OnChain.hs#L209 "poll manager validateMergeShardsManager") for the [poll manager](https://github.com/IndigoProtocol/smart-contracts/blob/532d8cb96e81955a812d823417b742c4f6415f4a/src/Indigo/Contracts/Governance/Poll/OnChain.hs#L70 "poll manager validator")) to combine all votes, and the shards are destroyed along with the poll tokens.

Multiple polls can be active at the same time (polls are created through [`validateCreateProposal`](https://github.com/IndigoProtocol/smart-contracts/blob/532d8cb96e81955a812d823417b742c4f6415f4a/src/Indigo/Contracts/Governance/Gov/OnChain.hs#L63)), each with their own set of poll shards. Incorrect validation allows for the merge operation to be performed for one poll with a set of poll shards from an entirely separate poll. This results in no restrictions being placed on the output location of the poll shards' values, allowing an attacker to steal the poll tokens and _Ada_ from the poll shards. Further, the poll to which the poll shards actually belonged is now invalidated and is doomed to [conclude through expiration](https://github.com/IndigoProtocol/smart-contracts/blob/532d8cb96e81955a812d823417b742c4f6415f4a/src/Indigo/Contracts/Governance/Poll/OnChain.hs#L423-L428), as its poll shards cannot be tallied.

With poll tokens under an attacker's control, they [can create more "counterfeit" poll tokens](https://github.com/IndigoProtocol/smart-contracts/blob/532d8cb96e81955a812d823417b742c4f6415f4a/src/Indigo/Data/Token.hs#L87-L92), and fake poll shards and poll managers to make any changes they desire to the protocol.

## How to reproduce

To test for the vulnerability it is sufficient to create two polls, wait for one of them to conclude, and submit a transaction as shown [here](https://github.com/IndigoProtocol/smart-contracts/blob/29e3d2abf558bc2e51e1c9ab63c78b28040bb719/tests/Spec/Governance/Transactions.hs#L326).

A [test](https://github.com/IndigoProtocol/smart-contracts/blob/29e3d2abf558bc2e51e1c9ab63c78b28040bb719/tests/Spec/Governance/Benchmark.hs#L220) was added in the IndigoProtocol/smart-contracts test suite that demonstrates the attack. To reproduce it is sufficient to run the following steps:

```bash
    $ git clone https://github.com/IndigoProtocol/smart-contracts.git
    $ cd smart-contracts
    $ git switch audit/merge-shards-exploit
    $ nix develop
    $ cabal run -f-plutonomy indigo-tests -- --test -- -p "/Merge shards exploit succeeds/"
    Up to date
    benchmark test suite
      Governance
        Create Asset
          Merge shards exploit succeeds: OK (0.37s)
        Upgrade Protocol
          Merge shards exploit succeeds: OK (0.14s)
        Text Proposal
          Merge shards exploit succeeds: OK (0.16s)
        Delist Asset
          Merge shards exploit succeeds: OK (0.28s)
        Migrate Asset
          Merge shards exploit succeeds: OK (0.26s)
        Modify Protocol params
          Merge shards exploit succeeds: OK (0.14s)

    All 6 tests passed (1.34s)
```

## Recommendation

[A PR](https://github.com/IndigoProtocol/smart-contracts/pull/471) with the recommended fix was provided during the audit, and contains corresponding test cases.

## References

1. [CVSS 3.1 Qualitative Severity Rating Scale](https://www.first.org/cvss/v3.1/specification-document)

2. [MLabs vulnerability classification](https://www.notion.so/Vulnerability-Types-ad39253c84ce443a82b835d94d765ba2)

3. [Indigo Yellow Paper](https://indigoprotocol.io/wp-content/uploads/2022/01/yellowpaper.pdf)

4. [Audit Merge Shards Exploit #450](https://github.com/IndigoProtocol/smart-contracts/pull/450)

5. [Fix for #450 - #471](https://github.com/IndigoProtocol/smart-contracts/pull/471)

6. Vulnerable validators:

   1. [`validateMergeShards`](https://github.com/IndigoProtocol/smart-contracts/blob/58d2250067f91f6d424d52e08198e2688036d84d/src/Indigo/Contracts/Governance/Poll/OnChain.hs#L174)

   2. [`validateMergeShardsManager`](https://github.com/IndigoProtocol/smart-contracts/blob/58d2250067f91f6d424d52e08198e2688036d84d/src/Indigo/Contracts/Governance/Poll/OnChain.hs#L208)

7. [`validatePoll`](https://github.com/IndigoProtocol/smart-contracts/blob/532d8cb96e81955a812d823417b742c4f6415f4a/src/Indigo/Contracts/Governance/Poll/OnChain.hs#L58 "validatePoll validator")

8. [`validateCreateProposal`](https://github.com/IndigoProtocol/smart-contracts/blob/532d8cb96e81955a812d823417b742c4f6415f4a/src/Indigo/Contracts/Governance/Gov/OnChain.hs#L63)

9. [`validatePollManager`](https://github.com/IndigoProtocol/smart-contracts/blob/532d8cb96e81955a812d823417b742c4f6415f4a/src/Indigo/Contracts/Governance/Poll/OnChain.hs#L70 "poll manager validator")

10. [Poll token asset class](https://github.com/IndigoProtocol/smart-contracts/blob/532d8cb96e81955a812d823417b742c4f6415f4a/tests/Utils/Mock.hs#L89 "poll token asset class")

11. [Exploit transaction example](https://github.com/IndigoProtocol/smart-contracts/blob/29e3d2abf558bc2e51e1c9ab63c78b28040bb719/tests/Spec/Governance/Transactions.hs#L326)

12. [Exploit test case](https://github.com/IndigoProtocol/smart-contracts/blob/29e3d2abf558bc2e51e1c9ab63c78b28040bb719/tests/Spec/Governance/Benchmark.hs#L220)
