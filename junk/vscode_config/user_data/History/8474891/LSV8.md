title: Indigo Audit Report
date: 2022-10-xx
author: MLabs

---
# Audit Report
**Indigo On-Chain Protocol**

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Audit Report](#audit-report)
    - [1. Warranty and Scope](#1-warranty-and-scope)
        - [1.1 Warranty](#11-warranty)
        - [1.2 Scope](#12-scope)
    - [2. Audit](#2-audit)
        - [2.1 Methodology](#21-methodology)
        - [2.2 Module specific analyis](#22-module-specific-analyis)
        - [2.2.1 CDP](#221-cdp)
        - [2.3 Findings](#23-findings)
            - [2.3.1 Template Finding](#231-template-finding)
    - [3. Conclusion](#3-conclusion)

<!-- markdown-toc end -->

## 1. Warranty and Scope

### 1.1 Warranty

The following report is presented without any guarantee or warranty. The report
lists all the issues found by MLabs during the Audit process.

### 1.2 Scope

MLabs has inspected exclusively the on-chain validation code provided by Indigo.
This excludes any off-chain code or component. During the audit, MLabs has
inspected the code contained in the provided files and attempted to locate
problems that can be found in the following categories:

1. Unclear or wrong specifications, that could lead to unwanted behaviour.
2. Wrongful implementation.
3. Vulnerabilities that can be leveraged by an attacker.
4. Code quality concerns and comments.

Where possible, MLabs have provided recommendations to address the relevant issues.

## 2. Audit

### 2.1 Methodology

MLabs analysed the validator scripts from the Indigo git remote
`https://github.com/IndigoProtocol/smart-contracts/` starting with commit
`bcf9ed05` - later updated to commit `82e69a21` at the request of the
client. During the process the found issues and vulnerabilities were posted
directly to the repository and filed as issues tracked under a meta _Audit
Issue_ with number:
[#439](https://github.com/IndigoProtocol/smart-contracts/issues/439). Some of
the findings were accompanied by additional tests, or proofs of behaviour to the
aforementioned repository.

MLabs spent the first sprint in an exploratory phase of the protocol, looking
across all modules provided, with all auditors inspecting all modules. This
exploratory phase was followed by a more structured phase, in which auditors
were assigned specific modules to inspect in detail and create local audit
reports, presented in section [2.2 Module specific
analyis](#22-module-specific-analyis).

To leverage a standardised metric for the severity of the open standard [Common
Vulnerability Scoring System](https://www.first.org/cvss/), together with the
[NVD Caluclator](https://nvd.nist.gov/vuln-metrics/cvss/v3-calculator). The
metrics from the mentioned tools were included with each vulnerability. MLabs
recognises that some of the parameters are not conclusive for the protocol - but
considers that leveraging such a standard is still valuable to offer a more
unbiased severity metric to found vulnerabilities.

### 2.2 Module specific analyis

### 2.2.1 CDP

- **Description**

- **Hypotheses**

### 2.3 Findings

#### 2.3.1 Template Finding

- **Severity:**
[6.7](https://nvd.nist.gov/vuln-metrics/cvss/v3-calculator?vector=AV:A/AC:H/PR:L/UI:N/S:U/C:L/I:H/A:H&version=3.1)

- **Vulnerability type:**

<!-- 
List of tags of vulnerability type, from [notion document](https://www.notion.so/Vulnerability-Types-ad39253c84ce443a82b835d94d765ba2). 
If a new typology is found, create its tag on the notion document and add to list. 

Example:
  - other-redeemer
  - foo 
  - bar
   -->

- **Description:**

<!-- The contract does X when it should be doing Y -->

- **How To Reproduce**

<!-- If a user would want to extract X then they can liquidate the whole portfolio -->

- **Provided Proof**

<!-- None, or test, or line number that causes the vulnerability.--> 

- **Recommended Fix**

<!-- Change foo, add bar -->

- **Links**

<!-- Refer to yellow paper, or link issue number, or link discussion --> 

#### 2.3.2 Incorrect validation of CDP liquidation transactions allows burning arbitrary amount of iAsset from the Stability Pool

| Severity | CVSS | Vulnerability type |
|---|---|---|
| <span style="color:red">Critical</span> | [9.5](https://nvd.nist.gov/vuln-metrics/cvss/v3-calculator?vector=AV:N/AC:L/PR:N/UI:N/S:C/C:N/I:H/A:N/E:H/RL:O/RC:C/CR:X/IR:H/AR:M/MAV:N/MAC:L/MPR:N/MUI:N/MS:C/MC:X/MI:H/MA:X&version=3.1) | incorrect-validation|

##### Description

Incorrect validation of CDP liquidation transactions allows anyone to burn arbitrary amount of iAsset from the Stability Pool.

Frozen CDPs are eligible for 'liquidation' by a Stability Pool associated with the same iAsset. CDP liquidation transactions come in two form; a full liquidation transaction that liquidates all of the minted iAssets associated with a CDP and a partial liquidation transaction that liquidates only a part of the minted iAssets associated with a CDP. 

An incorrect validation of full liquidation transactions was identified that allows anyone to burn arbitrary amount of iAsset from the Stability Pool regardless of the amount of iAsset actually minted by the CDP being liquidated.

For example, If a frozen CDP has 1 minted iAsset, and the Stability Pool has 1_000_000 iAsset staked. Then a full liquidation transaction can be submitted that burns all 1_000_000 iAsset from the Stability Pool.

##### How To Reproduce

To test for the vulnerability it's sufficient to build and submit a full CDP liquidation transaction that burns more of iAsset than it's minted by the frozen CDP.

A [test](https://github.com/IndigoProtocol/smart-contracts/blob/audit/liquidation-exploit/tests/Spec/CDP/Benchmark.hs#L247) was added in the IndigoProtocol/smart-contracts test suite that demonstrates the attack. To reproduce it's sufficient to run the following steps:

```bash
$ git clone https://github.com/IndigoProtocol/smart-contracts.git
$ git switch audit/liquidation-exploit
$ nix develop
$ cabal run -f-plutonomy indigo-tests -- --test -- -p "/liquidate exploit succeeds/"
Up to date
benchmark test suite
  CDP
    liquidate exploit succeeds: OK (0.39s)

All 1 tests passed (0.39s)
```
##### Recommendation

The validation of full liquidation transactions should include a check to ensure that the burned iAsset amount doesn't exceed the amount of iAsset minted by the CDP being liquidated.

##### References

1. [Severity Levels for Security Issues](https://www.atlassian.com/trust/security/security-severity-levels)
2. [MLabs vulnerability classification](https://www.notion.so/Vulnerability-Types-ad39253c84ce443a82b835d94d765ba2)
3. [Audit - Liquidity Pool Liquidation Exploit #441](https://github.com/IndigoProtocol/smart-contracts/pull/441)
4. [Liquidation exploit test](https://github.com/IndigoProtocol/smart-contracts/blob/audit/liquidation-exploit/tests/Spec/CDP/Benchmark.hs#L247)

#### 2.3.3 Missing integration tests with the real Cardano environment risks unexpected issues during deployment

| Severity | CVSS | Vulnerability type |
|---|---|---|
| Low | [6.7](https://nvd.nist.gov/vuln-metrics/cvss/v3-calculator?vector=AV:A/AC:H/PR:L/UI:N/S:U/C:L/I:H/A:H&version=3.1) | not-a-vulnerability |

##### Description

Currently, Indigo's test suite uses the [Plutus Simple Model](https://github.com/mlabs-haskell/plutus-simple-model) testing framework, which is similar to emulator traces and is used to estimate program resource usage and assert correctness conveniently and quickly. 

However, testing and running Plutus programs in a real Cardano network is essential as various issues can arise when dealing with not deterministic parts of the blockchain like contention, slot timing, block validation, etc.

##### Recommendation

Test the Indigo Plutus program behaviour using any of the available integration test frameworks, specifically [Plutip](https://github.com/mlabs-haskell/plutip) or using [CTL integration with Plutip](https://github.com/Plutonomicon/cardano-transaction-lib/blob/develop/doc/plutip-testing.md).

##### References

1. [Plutip](https://github.com/mlabs-haskell/plutip)
2. [Plutus Simple Model](https://github.com/mlabs-haskell/plutus-simple-model)
3. [cardano-transaction-library](https://github.com/Plutonomicon/cardano-transaction-lib)
4. [CTL integration with Plutip](https://github.com/Plutonomicon/cardano-transaction-lib/blob/develop/doc/plutip-testing.md)
5. [Severity Levels for Security Issues](https://www.atlassian.com/trust/security/security-severity-levels)
6. [MLabs vulnerability classification](https://www.notion.so/Vulnerability-Types-ad39253c84ce443a82b835d94d765ba2)

#### 2.3.4 Missing check in the Poll validator to ensure that inputs from other scripts are spent with the correct redeemer is unsafe

| Severity | CVSS | Vulnerability type |
|---|---|---|
| Low | [6.7](https://nvd.nist.gov/vuln-metrics/cvss/v3-calculator?vector=AV:A/AC:H/PR:L/UI:N/S:U/C:L/I:H/A:H&version=3.1) | other-redeemer |

##### Description


The *Other Redeemer* vulnerability was identified in the Poll validator and specifically the `Indigo.Contracts.Governance.Poll.OnChain.validateVote` function that validates spending from the Poll validator with the Poll's `Vote` redeemer.
The function implicitly relies that the transaction spends an input from the Staking validator with the Staking's `Lock` redeemer, but performs no explicit checks to ensure it, which is considered an unsafe practice.

In the future, if the Poll validator is modified it could inadvertently introduce a vulnerability.

In the pre audit engagement, a vulnerability of this type was indeed [found and reported](https://github.com/IndigoProtocol/smart-contracts/issues/416). 

##### How to reproduce

No exploit for the vulnerability was identified during the audit.

However, hypothetically speaking, if a Staking validator redeemer was available for use such that the vulnerable `Indigo.Contracts.Governance.Poll.OnChain.validateVote` still successfully validated, the vulnerability could become exploitable as some checks in `Indigo.Contracts.Staking.OnChain.validateLock` would not be performed.

##### Recommendation

Add an explicit check in the `Indigo.Contracts.Governance.Poll.OnChain.validateVote` function using the `Indigo.Utils.Helpers.usesSpendRedeemer` function to assert that the transaction spends an input from the Staking validator using the correct Staking's `Lock` redeemer.

##### References

1. [Poll validator](https://github.com/IndigoProtocol/smart-contracts/blob/audit/main/src/Indigo/Contracts/Governance/Poll/OnChain.hs#L51)
2. [Poll validateVote](https://github.com/IndigoProtocol/smart-contracts/blob/audit/main/src/Indigo/Contracts/Governance/Poll/OnChain.hs#L51)
3. [Staking validator](https://github.com/IndigoProtocol/smart-contracts/blob/audit/main/src/Indigo/Contracts/Staking/OnChain.hs#L35)
4. [Staking validateLock](https://github.com/IndigoProtocol/smart-contracts/blob/audit/main/src/Indigo/Contracts/Staking/OnChain.hs#L382)
5. [Severity Levels for Security Issues](https://www.atlassian.com/trust/security/security-severity-levels)
6. [MLabs vulnerability classification](https://www.notion.so/Vulnerability-Types-ad39253c84ce443a82b835d94d765ba2)


#### 2.3.5 No lower bound for CDP positions can disincentivise CDP liquidation

| Severity | CVSS | Vulnerability type |
|---|---|---|
| Critical | [6.7](https://nvd.nist.gov/vuln-metrics/cvss/v3-calculator?vector=AV:A/AC:H/PR:L/UI:N/S:U/C:L/I:H/A:H&version=3.1) | missing-incentive |

##### Description

Indigo protocol allows creation of arbitrarily small CDPs which could disincentivise freezing and liquidation of such CDPs and in turn compromise the minimal collateral ratio goal.

This happens when the collateral surplus contained within a CDP that liquidators hope to acquire is smaller than the total transaction cost of freezing and liquidating the CDP. Effectively, the liquidation would incur a net loss on the Stability Provider that is trying to liquidate the CDP.

##### How to reproduce

Consider the following scenario:

1. The price ratio of IUSD to Ada is 1:1 (i.e. 1 IUSD = 1 Ada).
2. The MCR for opening the IUSD CDP is +50% (i.e. to mint 1 IUSD user must lock 1.5 Ada as collateral).
3. The Tx fee to freeze and liquidate a CDP is 1 Ada each.

Alice opens an IUSD CDP by depositing 3 Ada as collateral and mints 2 IUSD.

Now the price of IUSD changes to 1 IUSD = 1.25 Ada. Which leads to Alice's CDP becoming under-collateralized.

Bob who has an account in IUSD stability pool and owns 10% of the IUSD present in the stability pool, wants to freeze and liquidate Alice's CDP but if he would liquidate the CDP then he would incur a net loss. Here's why:

1. Bob would have to submit a Tx that freezes Alice's CDP which would cost him 1 Ada as Tx fee.
2. Then Bob would have to again submit a Tx liquidating the freezed CDP which would also cost him 1 Ada as Tx fee.
3. When the liquidation is done which depletes the stability pool by 10%, Bob would loose 0.25 Ada worth of IUSD. And he would gain 0.3 Ada as the reward for liquidating the CDPs.

Hence, the total cost incur by Bob is -2.25 Ada and the reward gained is 0.3 Ada. Therefore, bob would incur a net loss of -1.95 Ada.

The above scenario describes one of the following cases where creating arbitrarily small CDPs de-incentivize the liquidators to liquidate the CDPs even when they went under-collateralized. 

##### **Recommended Fix**

The fix for this missing-incentive problem requires careful balance between:

1. Users who owns some substantial amount of IAsset present in the stability pool
2. Typical percent of stability pool depletion when liquidating a CDP.
3. The price at which liquidation will occur.

Incentivizing every user of the stability pool is not particularly good, because some users may own IAssets in the stability pool that's very close to zero (something like: 0.000001%), hence there reward when liquidating a CDP will also be close to zero (R * 0.000001) unless the CDP whose liquidation in performed contains massive amount of collateral. Therefore, if we want to incentivize such users to liquidate the CDPs then the lower bound for liquidating the CDP will be very high, which means very few people will be able to open the CDP.

Hence, we want to find a good balance between users that owns at least some substantial amount of IAsset present in the stability pool (something like: 1%) and create a lower bound based on that. Such that every user who owns at least 1% of the stability pool is guaranteed to have a profit when liquidating a CDP.

One way to achieve this is by using the following formula:

```latex
R - (P + F) > 0
```

Where

  N = amount of IAssets

  F = Tx Fee for liquidation of CDP + Tx Fee for Freezing the CDP

  P = Price of IAsset at which liquidation will occur * Depletion percent of stability pool * N

  R = MCR * Price of the IAsset * Min percent of stability pool a user should own * N

Now, we solve for N, by substituting appropriate values in the equation.


##### References

1. [Severity Levels for Security Issues](https://www.atlassian.com/trust/security/security-severity-levels)
2. [MLabs vulnerability classification](https://www.notion.so/Vulnerability-Types-ad39253c84ce443a82b835d94d765ba2)


#### 2.3.4 Missing check in the CDP validator to ensure that inputs from other scripts are spent with the correct redeemer is unsafe

| Severity | CVSS | Vulnerability type |
|---|---|---|
| Low | [6.7](https://nvd.nist.gov/vuln-metrics/cvss/v3-calculator?vector=AV:A/AC:H/PR:L/UI:N/S:U/C:L/I:H/A:H&version=3.1) | other-redeemer |

##### Description

The *Other Redeemer* vulnerability was identified in the CDP validator and specifically the `Indigo.Contracts.CDP.OnChain.validateLiquidateCDP` function that validates spending from the CDP validator with the CDP's `Liquidate` redeemer.
The function implicitly relies that the transaction spends an input from the Stability Pool validator with the Stability Pool's `LiquidateCDP` redeemer, but performs no explicit checks to ensure it, which is considered an unsafe practice.

In the future, if the Stability Pool validator is modified it could inadvertently introduce a vulnerability.

In the pre audit engagement, a vulnerability of this type was indeed [found and reported](https://github.com/IndigoProtocol/smart-contracts/issues/416). 

##### How to reproduce

No exploit for the vulnerability was identified during the audit.

However, hypothetically speaking, if a Stability Pool validator redeemer was available for use such that the vulnerable `Indigo.Contracts.CDP.OnChain.validateLiquidateCDP` still successfully validated, the vulnerability could become exploitable as some checks in `Indigo.Contracts.StabilityPool.OnChain.validateLiquidateCDP` would not be performed.

##### Recommendation

Add an explicit check in the `Indigo.Contracts.CDP.OnChain.validateLiquidateCDP` function using the `Indigo.Utils.Helpers.usesSpendRedeemer` function to assert that the transaction spends an input from the Stability Pool validator using the correct Stability Pool's `LiquidateCDP` redeemer.

##### References

1. [CDP validator](https://github.com/IndigoProtocol/smart-contracts/blob/audit/main/src/Indigo/Contracts/CDP/OnChain.hs#L657)
2. [CDP validateLiquidateCDP](https://github.com/IndigoProtocol/smart-contracts/blob/audit/main/src/Indigo/Contracts/CDP/OnChain.hs#L318)
3. [Stability Pool validator](https://github.com/IndigoProtocol/smart-contracts/blob/audit/main/src/Indigo/Contracts/StabilityPool/OnChain.hs#L64)
4. [Stability Pool validateLiquidateCDP](https://github.com/IndigoProtocol/smart-contracts/blob/audit/main/src/Indigo/Contracts/StabilityPool/OnChain.hs#L205)
5. [Severity Levels for Security Issues](https://www.atlassian.com/trust/security/security-severity-levels)
6. [MLabs vulnerability classification](https://www.notion.so/Vulnerability-Types-ad39253c84ce443a82b835d94d765ba2)

#### 2.3.5 Incorrect validation of Proposal execution allows stealing of the Upgrade Token.

| Severity | CVSS | Vulnerability type |
|---|---|---|
| <span style="color:red">Critical</span> | [9.3](https://nvd.nist.gov/vuln-metrics/cvss/v3-calculator?vector=AV:N/AC:H/PR:N/UI:N/S:C/C:N/I:H/A:N/E:F/RL:O/RC:C/CR:X/IR:H/AR:M/MAV:N/MAC:L/MPR:N/MUI:N/MS:C/MC:X/MI:H/MA:X&version=3.1) | multiple-satisfaction |

##### Description

If there are two successful proposals, then a user can steal an upgrade token by executing both of the proposals in the same transaction. 

A passed proposal results in an UTXO with an `Upgrade Token`. There are pairs of `Upgrade Tokens` whose validation logic allows for them to be executed simultaneously in a single transaction. For example two `Text Proposals` or two `Modify Protocol Params` proposals for the same parameters.

Though the validation logic checks precisely for a single `Upgrade Token` to get burnt. This allows for a transaction spending two `Upgrade Tokens`, burning one of them as demanded by the validator, but sending the remaining one to a wallet address of an attacker. This is very severe as a user having an upgrade token could execute arbitrary proposals.

##### How To Reproduce

To test for the vulnerability we use two successful `Text Proposals` and execute them in a single transaction. 

A [test](https://github.com/IndigoProtocol/smart-contracts/blob/3ceec2538fba0266b75c5e7cadddeae1fb8fb4e6/tests/Spec/Governance/Benchmark.hs#L197) was added in the IndigoProtocol/smart-contracts test suite that demonstrates the attack. To reproduce it's sufficient to run the following steps:

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

##### Recommendation

The validation of proposal execution should check for the unique update token in the inputs. Ability to execute multiple proposals at once is more confusing than usefull.

##### References

1. [Severity Levels for Security Issues](https://www.atlassian.com/trust/security/security-severity-levels)
2. [MLabs vulnerability classification](https://www.notion.so/Vulnerability-Types-ad39253c84ce443a82b835d94d765ba2)
3. [Audit - Proposal Exploit #466](https://github.com/IndigoProtocol/smart-contracts/pull/466)
4. [Proposal exploit test](https://github.com/IndigoProtocol/smart-contracts/blob/3ceec2538fba0266b75c5e7cadddeae1fb8fb4e6/tests/Spec/Governance/Benchmark.hs#L197)

## 3. Conclusion
