
1. cdp liquidate run with other redeemer in stability pool input:
- CreateAccount : can't because mint is negative for cdp, but 1 account for stability
- AdjustAccount
- LiquidateCD
- Clos
- SpendAccoun
- RecordEpochToScaleToSu
- UpgradeVersio

[WIP] Fix for #441, Exploit on Stability Pool liquidations - https://github.com/IndigoProtocol/smart-contracts/pull/443
[READY FOR REVIEW] Fix for #450, Exploit on Merge Shards - https://github.com/IndigoProtocol/smart-contracts/pull/471
[READY FOR REVIEW] Fix for #462, Missing Redeemer check - https://github.com/IndigoProtocol/smart-contracts/pull/472
[READY FOR REVIEW] Fix for #466, Exploit stealing upgrade token - https://github.com/IndigoProtocol/smart-contracts/pull/473
[READY FOR REVIEW] Fix for #446, No collector script tests - https://github.com/IndigoProtocol/smart-contracts/pull/479
[WIP] Fix for #469, No tests for Protocol Fee Distribution - https://github.com/IndigoProtocol/smart-contracts/pull/480
[READY FOR REVIEW] Add minimum collateral for CDPs - https://github.com/IndigoProtocol/smart-contracts/pull/482
[READY FOR REVIEW] Remove OracleAssetNFT and cleanup dead code - https://github.com/IndigoProtocol/smart-contracts/pull/476
[READY FOR REVIEW] Change Oracle Datum structure to match draft CIP - https://github.com/IndigoProtocol/smart-contracts/pull/474
