
# Description

Stability pools' purpose of existance in the protocol is to provide pools of iAssets ready to be used for liquidating debted CDPs.
Liquidating a CDP means burning same amount of iAssets as were previously minted, balancing the total supply. iAssets in liquidation are taken from a stability pool.

Liquidation is usually profitable (collected collateral is of greater value than burned iAssets). The purpose for user to delegate his funds to a staking pool is to participate in liquidation - the earned value is splitted among stability pool providers.

`StabilityPoolToken` marks pool output, containing iAssets and collateral Ada.

`StabilityPoolToken` gets minted together with `iAssetToken` at execute `ProposeAsset` endpoint and owned by StabilityPool script, `iAssetToken` by CDP script.

# Hypotheses

 - [ ] SP-UR-001 : As a user, I can open a Stability Pool account with my iAssets. A Stability Pool account
represents a user’s total deposited iAssets, and their amount of rewards available.
     - [X] User can open an account with iAssets
     - [ ] Stability pool account represents user's total deposited iAssets
 - [X] SP-UR-002 : As a user, I can deposit my iAssets into a Stability Pool.
 - [X] SP-UR-003 : As a user, I can withdraw my iAssets from a Stability Pool.
 - [X] SP-UR-004 : As a user, I can use a Stability Pool to liquidate an undercollateralized CDP.
 - [ ] SP-UR-005 : As a user, I can withdraw my rewards (in the form of ADA that was transferred from a
liquidated CDP’s collateral) from the Stability Pool.
92.2.2
 - [X] SP-PR-001 : The protocol dictates that a Stability Pool can only contain an iAsset that has been
    whitelisted.
    - 
 - [ ] SP-PR-002 : The protocol dictates that adjusting a Stability Pool account (deposit iAsset, withdraw
iAsset) redeems the account rewards.
 - [ ] SP-PR-003 : The protocol dictates that Stability Pool and Stability Pool Account can be upgraded via
a protocol upgrade.
 - [ ]  


# Hypotheses:
 1. Freeze
  - 

# Potential problems
 - [X] new iAsset and StabilityPoolToken if minted can remain unspent <- NOT true
 - [ ] ProposeAsset can mint multiple times utxos with the same new iAsset names, then i.e. liquidation possible using two different oracles, two stability pools
 - [ ] "3.1.7. 3 (d) The transaction cannot consume any Stability Pool input." <- unclear specification: it instead both consumes and outputs a stability pool output.
 - liquidation collateral doesn't pay transaction fee. Does someone need to volunteer for the whole pool and pay it out of his pocket?
 - stability pool liquidation iAssetValHash check
 - CDPScriptParams doubles spValHash information? as minting policy hash in stabilityPoolAuthToken and spValHash
 - `RecordEpochToScaleToSum` endpoint missing from specification

# Notes
 - newly created iAsset gets outputed to CDP script
 - newly created StabilityPoolToken gets outputed to StabilityPool script
 - https://medium.com/liquity/scaling-liquitys-stability-pool-c4c6572cf275
 - liquidation after freezing
 
# Questions
 
 - if liquidating is profitable and people would like to do it, then can't it be left up to volunteers to do? why stability pools are needed?
 - One `StabilityPoolToken` per iAsset right? <- Yes
 - What does dotted arrow mean in diagrams? <- reference inputs
 - whats the source of initial tokens for stability pools? not-yet-backed cdp's