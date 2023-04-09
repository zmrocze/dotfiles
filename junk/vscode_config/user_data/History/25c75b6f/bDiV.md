---
title: "**Indigo Audit 2**"
subtitle: "Audit Report"
author: "MLabs Audit Team"
date: \today
titlegraphic: "./linked-files/images/MLabs-logo.jpg"
logo: "./linked-files/images/MLabs-logo.jpg"
fontsize: 10
colorlinks: true
graphics: true
title-on: true
block-headings: true
numbersections: true

---
# Disclaimer

**This audit report is presented without warranty or guarantee of any type. Neither MLabs nor its auditors can assume any liability whatsoever for the use, deployment or operations of the audited code.** This report lists the most salient concerns that have become apparent to MLabs’ auditors after an inspection of the project's codebase and documentation, given the time available for the audit. Corrections may arise, including the revision of incorrectly reported issues. Therefore, MLabs advises against making any business or other decisions based on the contents of this report.

An audit does not guarantee security. Reasoning about security requires careful considerations about the capabilities of the assumed adversaries. These assumptions and the time bounds of the audit can impose realistic constraints on the exhaustiveness of the audit process. Furthermore, the audit process involves, amongst others, manual inspection and work which is subject to human error.

**MLabs does not recommend for or against the use of any work or supplier mentioned in this report.** This report focuses on the technical implementation provided by the project's contractors and subcontractors, based on the information they provided, and is not meant to assess the concept, mathematical validity, or business validity of their product. This report does not assess the implementation regarding financial viability nor suitability for any purpose. *MLabs does not accept responsibility for any loss or damage howsoever arising which may be suffered as result of using the report nor does it guarantee any particular outcome in respect of using the code on the smart contract.*


\newpage

# Background

## Scope 

The scope of the Audit is limited to the Indigo Vesting Script as introduced by [PR 542](https://github.com/IndigoProtocol/smart-contracts/pull/542) found in `https://github.com/IndigoProtocol/smart-contracts` repository. The git commit reference of the audited version is: `10b45df183fdaa541b3825214913331d74730507`.

During the audit, MLabs Audit Team (referred to as MLabs) have used the provided files to:

1. Code and logic review the protocol implementation.
2. Undertake an analysis based on _common vulnerability types_.
3. Attempted to integrate _Plutip_ testing framweork to conduct integration tests.   

## Methodology

### Timeline

In response to the above scope, the Audit process took two weeks, starting from _2023-Jan-07_.  

### Information

#### Checksums

In addition to the protocol files, Indigo also provided a Google docs document titled [_Indy-Rewards Vesting Script Specification_](https://docs.google.com/document/d/1CLbUIr6FAAnGDrvHZCm0p20Ghne8v5MRB3YsFpJf2Mo/edit) with hash `a933...d70b`, snapshot as `docs/Indy-rewards-vesting-script-specification.md` in the audit repository. 

### Audit Report

The audit report is an aggregation and résumé of the found issue. More extensive tickets were created in the _Indigo_ repository - expanding on each of the findings present in this report. 

### Metrics

#### CVSS

To leverage a standardised metric for the severity of the open standard [Common Vulnerability Scoring System][cvss], together with the [NVD Calculator][nvd-1]. The metrics from the mentioned tools were included with each vulnerability. MLabs recognises that some of the parameters are not conclusive for the protocol - but considers that leveraging such a standard is still valuable to offer a more unbiased severity metric for the found vulnerabilities.

#### Severity Levels

The aforementioned CVSS calculations were then benchmarked using the [CVSS-Scale][cvss-scale] metric, receiving a grade spanning from `Low` to `Critical`. This additional metric allows for an easier, human understandable grading, whilst leveraging the CVSS standardised format.

<!-- Refs -->
[cvss-scale]: https://www.first.org/cvss/v3.1/specification-document#Qualitative-Severity-Rating-Scale "CVSS-Scale"
[cvss]: https://www.first.org/cvss/ "Common Vulnerability Scoring System"
[nvd-1]: https://nvd.nist.gov/vuln-metrics/cvss/v3-calculator "NVD Calculator"


\newpage

# Findings & Recommendations

## Findings

### Hypotheses tested

- [ ] _Validate that the vesting schedule specified in the specification matches the vesting schedule in the vesting contract_ 

- [ ] _ Unbounded protocol datum in the vesting unlocking output. _

### Vulnerability types considered

- [x] `other-redeemer`

Redeemer of the validated input is checked. No other validator gets involved. 
Vulnerability doesn't exist.

- [x] `other-token-names`

Indy cannot get minted, so vulnerability doesn't apply.

- [x] `unbounded-protocol-datum`

Covered by arbitrary-utxo-datum.

- [ ] `arbitrary-utxo-datum`

The output of a transaction unlocking vesting funds that goes to the distributor is not checked for any specific datum.
This allows creation of an unspendable output due to increased transaction execution units, effectively burning the INDY funds.
Vulnerability found and reported [here](https://github.com/mlabs-haskell/smart-contracts-audit-2/issues/7).

- [x] `unbounded-protocol-value`

INDY tokens cannot get minted after the initial mint. Their constant amount in circulation is checked for by the vesting validator.
No vulnerability.

- [x] `foreign-utxo-tokens`

The vesting validator checks for existance of UTXOs with precisely the expected INDY tokens and no other tokens.
No vulnerability.

- [x] `multiple-satisfaction`

The vesting validator ensures only a single UTXO with INDY gets spent.  
No vulnerability.

- [x] `multiple-mint-satisfaction`

Covered by multiple-satisfaction.

- [x] `locked-ada`

The protocol doesn't require creation of any unspendable Ada outputs.
No vulnerability.

- [x] `locked-nonada-values`

The vesting validator allows for up to two outputs with INDY tokens. It checks if together they hold exactly as much INDY as gets spent in the transaction.
No vulnerability.

- [x] `missing-utxo-authentication`

The protocol mantains a single INDY utxo at the vesting script address, given that the distributor is not the vesting script itself. The validator has to spend that and only that INDY input.
No vulnerability.

- [x] `missing-incentive`

There is incentive for anyone profiting from the vesting funds to unlock them with the vesting contract.
No vulnerability.

- [x] `bad-incentive`

Not found.

- [x] `utxo-contention`

The protocol uses a single global utxo, though vesting funds can be unlocked only once every 5 days, so contention is not any problem.
No vulnerability.

- [x] `cheap-spam`

Not found.

- [ ] `insufficient-tests`

Missing integration tests risk unexpected behaviour on the real network. Vulnerability was reported [here](https://github.com/mlabs-haskell/smart-contracts-audit-2/issues/10).

- [ ] `incorrect-documentation`

Five days is the current value of an Epoch but this doesn't mean it would change in a future hard fork. Reported [here](https://github.com/mlabs-haskell/indigo-smart-contracts-audit-2/issues/9)

- [x] `insufficient-documentation`

Not found.

- [x] `insufficient-staking-control`

The vesting validator expects two outputs. Distributor output is sent to an address. Continuing output address can't contain staking credentials. Both of the outputs have fixed staking credentials.
No vulnerability.

- [x] `incorrect-logic`

Not found.

- [x] `unchecked-datum`

Not found.

- [x] `infinite-minting`

Vesting validator is not a minting policy.
No vulnerability.

- [x] `division-by-zero`

Every division in the vesting validator is by a positive constant.
No vulnerability.

- [x] `rounding-error`

Not found in the locked/unlocked funds calculations.

- [x] `sybil-contention`

Doesn't apply to the vesting validator.
Not found.

- [x] `poor-code-standards`

Not found.

## Recommendations

- [ ] _Integration Tests Missing_

- [ ] _Ambiguous definition of vesting releasing interval [Incorrect documentation]_ 


\newpage

# Vulnerabilities

##  Unbounded protocol datum in the vesting unlocking output. 

### Unbounded protocol datum in the vesting unlocking output. 
An `unbounded-protocol-datum` vulnerability was found:  [link](https://github.com/IndigoProtocol/smart-contracts/issues/553).


## Validate that there isn't double spending of Indy in the same epoch

#### Description

The vesting contract used by indigo protocol contains 3 hard-coded distribution schedule for stability pool stakers, voters and the DEX liquidity providers. These distribution scheduled has been specified in the following [specification](https://docs.google.com/document/d/1CLbUIr6FAAnGDrvHZCm0p20Ghne8v5MRB3YsFpJf2Mo/). From the specified date in the distribution schedules every cardano epoch specified amount of indy is to be vested.

#### Goal

Validate that there isn't a multiple spending of indy in the same cardano epoch i.e. there isn't a way to make multiple vesting in same cardano epoch.

#### Method

#### Conclusion


#### Links

- meta-task _Hypotheses Template_ 
- [specification for the vesting contract](https://docs.google.com/document/d/1CLbUIr6FAAnGDrvHZCm0p20Ghne8v5MRB3YsFpJf2Mo/)


## Ambiguous definition of vesting releasing interval [Incorrect documentation]

#### Description

The specs say `beginning from the specified date, every Cardano epoch (5 days) the specified amount of INDY is vested`. 

Since we spect to run the vesting for 5 years we cannot guarantee that the length of an epoch would be 5 days in the following hard forks as documented in [1]. Right now the code uses directly 5 days.

#### Goal

Clarify if the intention is to use always an epoch length or if it is to use 5 days.

#### Conclusion


#### Links

- [1] https://github.com/input-output-hk/ouroboros-network/blob/c65353299ff3efde05bf07d628a2ac7ea3193458/ouroboros-consensus/src/Ouroboros/Consensus/HardFork/History/EraParams.hs#L132
- meta-task _Hypotheses Template_ 
- [specification for the vesting contract](https://docs.google.com/document/d/1CLbUIr6FAAnGDrvHZCm0p20Ghne8v5MRB3YsFpJf2Mo/)


## Integration Tests Missing

| Severity | CVSS | Vulnerability type |
| -------- | ---- | ------------------ |
| None     | N/A  | [insufficient-tests](#insufficient-tests) |

#### Description

Vesting contract tests are currently conducted using [plutus-simple-model](https://github.com/mlabs-haskell/plutus-simple-model) testing framework, a very similar framework to the emulator traces provided by [plutus-apps](https://github.com/input-output-hk/plutus-apps). The [plutus-simple-model](https://github.com/mlabs-haskell/plutus-simple-model) testing framework is primarily used to estimate resource requirements for smart contracts running on the Cardano blockchain and it is a convenient and fast way to test the properness of smart contracts.

However, it is imperative to test and run Plutus programs in a real Cardano network in order to address any issues that may arise when dealing with non-deterministic elements of the blockchain, such as contention, slot timing, block validation, etc. The importance of this can be further illustrated by the fact that vesting contracts heavily rely on POSIXTime and epoch of Cardano network.

#### Recommendation

Test the behaviour of vesting contract using any of the available integration test frameworks, specifically [Plutip](https://github.com/mlabs-haskell/plutip) or using [CTL integration with Plutip](https://github.com/Plutonomicon/cardano-transaction-lib/blob/develop/doc/plutip-testing.md).

#### References

1. [CVSS 3.1 Qualitative Severity Rating Scale](https://www.first.org/cvss/v3.1/specification-document#Qualitative-Severity-Rating-Scale)
2. [MLabs vulnerability classification](https://www.notion.so/Vulnerability-Types-ad39253c84ce443a82b835d94d765ba2)
3. [Plutip](https://github.com/mlabs-haskell/plutip)
4. [plutus-apps](https://github.com/input-output-hk/plutus-apps)
5. [Plutus Simple Model](https://github.com/mlabs-haskell/plutus-simple-model)
6. [cardano-transaction-library](https://github.com/Plutonomicon/cardano-transaction-lib)
7. [CTL integration with Plutip](https://github.com/Plutonomicon/cardano-transaction-lib/blob/develop/doc/plutip-testing.md)


\newpage

# Appendix

<!-- Use the tags to link vulnerability types -->

## Vulnerability types

The following list of vulnerability types represents a list of commonly found vulnerabilities in Cardano smart contract protocol designs or implementations. The list of types is actively updated and added to as new vulnerabilities are found.

<a name="other-redeemer"></a>

### Other redeemer

**ID:** other-redeemer

**Test:** Transaction can avoid some checks when it can successfully spend a UTxO or mint a token with a redeemer that some script logic didn’t expect to be used.

**Property:** A validator/policy should check explicitly whether the ‘other’ validator/policy is invoked with the expected redeemer.

**Impacts:**

- Bypassing checks

<a name="other-token-names"></a>

### Other token name

**ID:** other-token-names

**Test:** Transaction can mint additional tokens with some ‘other’ token name of ‘own’ currency alongside the intended token name.

**Property:** A policy should check that the total value minted of their ‘own’ currency symbol doesn’t include unintended token names.

**Impacts:**

- Stealing protocol tokens
- Unauthorised protocol actions

**Example:**

A common coding pattern that introduces such a vulnerability can be observed in the following excerpt:

```haskell
vulnPolicy rmr ctx = do
  …
  assetClassValueOf txInfoMint ownAssetClass == someQuantity
  …
```

The recommended coding pattern to use in order to prevent such a vulnerability can be observed in the following excerpt:

```haskell
safePolicy rmr ctx = do
  …
  txInfoMint == (assetClassValue ownAssetClass someQuantity)
  …
```

<a name="unbounded-protocol-datum"></a>

### Unbounded Protocol datum

**ID:** unbounded-protocol-datum

**Test:** Transaction can create protocol UTxOs with increasingly bigger protocol datums.

**Property:** A protocol should ensure that all protocol datums are bounded within reasonable limits.

**Impacts:**

- Script XU and/or size overflow
- Unspendable outputs
- Protocol halting

**Example:**

A common design pattern that introduces such vulnerability can be observed in the following excerpt:

```haskell
data MyDatum = Foo {
  users :: [String],
  userToPkh :: Map String PubKeyHash
}
```

If the protocol allows these datums to grow indefinitely, eventually XU and/or size limits imposed by the Plutus interpreter will be reached, rendering the output unspendable.

The recommended design patterns is either to limit the growth of such datums in validators/policies or to split the datum across different outputs.

<a name="arbitrary-utxo-datum"></a>

### Arbitrary UTxO datum

**ID:** arbitrary-utxo-datum

**Test:** Transaction can create protocol UTxOs with arbitrary datums.

**Property:** A protocol should ensure that all protocol UTxOs hold intended datums.

**Impacts:**

- Script XU overflow
- Unspendable outputs
- Protocol halting

<a name="unbounded-protocol-value"></a>

### Unbounded protocol value

**ID:** unbounded-protocol-value

**Test:** Transaction can create increasingly more protocol tokens in protocol UTxOs.

**Property:** A protocol should ensure that protocol values held in protocol UTxOs are bounded within reasonable limits.

**Impacts:**

- Script XU overflow
- Unspendable outputs
- Protocol halting

<a name="foreign-utxo-tokens"></a>

### Foreign UTxO tokens

**ID:** foreign-utxo-tokens

**Test:** Transaction can create protocol UTxOs with foreign tokens attached alongside the protocol tokens.

**Property:** A protocol should ensure that protocol UTxOs only hold the tokens used by the protocol.

**Impacts:**

- Script XU overflow
- Unspendable outputs
- Protocol halting

<a name="multiple-satisfaction"></a>

### Multiple satisfaction

**ID:** multiple-satisfaction

**Test:** Transaction can spend multiple UTxOs from a validator by satisfying burning and/or paying requirements for a single input while paying the rest of the unaccounted input value to a foreign address.

**Property:** A validator/policy should ensure that all burning and paying requirements consider all relevant inputs in aggregate.

**Impacts:**

- Stealing protocol tokens
- Unauthorised protocol actions
- Integrity

**Example:**

A common coding pattern that introduces such a vulnerability can be observed in the following excerpt:

```haskell
vulnValidator _ _ ctx =
  ownInput ← findOwnInput ctx
  ownOutput ← findContinuingOutput ctx
  traceIfFalse “Must continue tokens” (valueIn ownInput == valueIn ownOutput)
```

Imagine two outputs at `vulnValidator` holding the same values

A. `TxOut ($FOO x 1 + $ADA x 2)` B. `TxOut ($FOO x 1 + $ADA x 2)`

A transaction that spends both of these outputs can steal value from one spent output by simply paying `$FOO x 1 + $ADA x 2` to the ‘correct’ address of the `vulnValidator`, and paying the rest `$FOO x 1 + $ADA x 2` to an arbitrary address.

<a name="locked-ada"></a>

### Locked Ada

**ID:** locked-ada

**Test:** Protocol locks Ada value indefinitely in obsolete validator outputs.

**Property:** Protocol should include mechanisms to enable redeeming any Ada value stored at obsolete validator outputs.

**Impacts:**

- Financial sustainability
- Cardano halting

<a name="locked-nonada-values"></a>

### Locked non Ada values

**ID:** locked-nonada-values

**Test:** Protocol indefinitely locks some non-Ada values that ought to be circulating in the economy.

**Property:** Protocol should include mechanisms to enable redeeming any non-Ada value stored at obsolete validator outputs.

**Impacts:**

- Financial sustainability
- Protocol halting

<a name="missing-utxo-authentication"></a>

### Missing UTxO authentication

**ID:** missing-utxo-authentication

**Test:** Transaction can perform a protocol action by spending or referencing an illegitimate output of a protocol validator.

**Property:** All spending and referencing of protocol outputs should be authenticated.

**Impacts:**

- Unauthorised protocol actions

**Example:**

Checking only for validator address and not checking for an authentication token.,

<a name="missing-incentive"></a>

### Missing incentive

**ID:** missing-incentive

**Test:** There is no incentive for users to participate in the protocol to maintain the intended goals of the protocol.

**Property:** All users in the Protocol should have an incentive to maintain the intended goals of the protocol

**Impacts:**

- Protocol stalling
- Protocol halting

<a name="bad-incentive"></a>

### Bad incentive

**ID:** bad-incentive

**Test:** There is an incentive for users to participate in the protocol that compromises the intended goals of the protocol.

**Property:** No users of the protocol should have an incentive to compromise the intended goals of the protocol.

**Impacts:**

- Protocol stalling
- Protocol halting

<a name="utxo-contention"></a>

### UTxO contention

**ID:** utxo-contention

**Test:** The protocol requires that transactions spend a globally shared UTxO(s) thereby introducing a contention point.

**Property:** The protocol should enable parallel transactions and contention-less global state management if possible.

**Impacts:**

- Protocol stalling
- Protocol halting

<a name="cheap-spam"></a>

### Cheap spam

**ID:** cheap-spam

**Test:** A transaction can introduce an idempotent or useless action/effect in the protocol for a low cost that can compromise protocol operations.

**Property:** The protocol should ensure that the cost for introducing a salient action is sufficient to deter spamming.

Severity increases when compounded with the `utxo-contention` vulnerability.

**Impacts:**

- Protocol stalling
- Protocol halting

<a name="insufficient-tests"></a>

### Insufficient tests

**ID:** insufficient-tests

**Test:** There is piece of validation logic that tests do not attempt to verify.

**Property:** Every piece of validator code gets meaningfully executed during tests.

**Impacts:**

- Correctness

<a name="incorrect-documentation"></a>

### Incorrect documentation

**ID:** incorrect-documentation

**Test:** There is a mistake or something confusing in existing documentation.

**Property:** Everything documented is clear and correct.

**Impacts:**

- Correctness
- Maintainability

<a name="insufficient-documentation"></a>

### Insufficient documentation

**ID:** insufficient-documentation

**Test:** There is a lack of important documentation.

**Property:** Everything of importance is documented.

**Impacts:**

- Comprehension
- Correctness

<a name="poor-code-standards"></a>

### Poor Code Standards

**ID:** poor-code-standards

**Test:** Missing the use of code quality and stadardisation tools. 

**Property:** Code is properly formatted, linted, and uses an adequate code standard.

**Impacts:**

- Codebase Maintainability 
- Comprehension


