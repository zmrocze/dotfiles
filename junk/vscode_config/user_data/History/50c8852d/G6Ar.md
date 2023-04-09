# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and we follow [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

**Table of Contents**

- [[Unreleased]](#-unreleased-)
  - [Added](#added)
  - [Changed](#changed)
  - [Removed](#removed)
  - [Fixed](#fixed)
- [[3.0.0] - 2022-11-21](#300---2022-11-21)
  - [Added](#added-1)
  - [Changed](#changed-1)
  - [Removed](#removed-1)
  - [Fixed](#fixed-1)
- [[2.0.0] - 2022-09-12](#200---2022-09-12)
  - [Added](#added-2)
  - [Changed](#changed-2)
  - [Removed](#removed-2)
  - [Fixed](#fixed-2)
- [[2.0.0-alpha] - 2022-07-05](#200-alpha---2022-07-05)
  - [Added](#added-3)
  - [Removed](#removed-3)
  - [Changed](#changed-3)
  - [Fixed](#fixed-3)
- [[1.1.0] - 2022-06-30](#110---2022-06-30)
  - [Fixed](#fixed-4)
- [[1.0.1] - 2022-06-17](#101---2022-06-17)
  - [Fixed](#fixed-5)
- [[1.0.0] - 2022-06-10](#100---2022-06-10)

## [Unreleased]

### Added

### Changed

### Removed
- Ctl haskell server, ability to apply scripts replaced by `Contract.Scripts.applyArgs`

### Fixed

## [3.0.0] - 2022-11-21

### Added

- Support passing the inital UTxO distribution as an Array and also get the KeyWallets as an Array when writing Plutip tests. ([#1018](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1018)). An usage example can be found [here](doc/plutip-testing.md).
- New `Contract.Test.Utils` assertions and checks: `assertOutputHasRefScript`, `checkOutputHasRefScript`, `checkTxHasMetadata` ([#1044](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1044))
- `Parallel` instance to `Contract` monad. Parallel capabilities are in the associated `ParContract` datatype ([#1037](https://github.com/Plutonomicon/cardano-transaction-lib/issues/1037))
- Balancer constraints interface (check [Building and submitting transactions](https://github.com/Plutonomicon/cardano-transaction-lib/blob/95bdd213eff16a5e00df82fb27bbe2479e8b4196/doc/getting-started.md#building-and-submitting-transactions) and `examples/BalanceTxConstraints.purs` for reference) ([#1053](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1053))
- New `Contract.Transaction` functions accepting balancer constraints as a parameter: `balanceTxWithConstraints`, `balanceTxsWithConstraints`, `withBalancedTxWithConstraints`, `withBalancedTxsWithConstraints` ([#1053](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1053))
- New functions `addressWithNetworkTagFromBech32` and `addressFromBech32` in `Contract.Address`, the second checking that address network Id corresponds to the contract environment network Id. ([#1062](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1062))
- `Contract.CborBytes` for CBOR-related functionality. ([#850](https://github.com/Plutonomicon/cardano-transaction-lib/issues/850))
- `ToData` & `FromData` instances for `PublicKey` in `Cardano.Types.Transaction` ([#998](https://github.com/Plutonomicon/cardano-transaction-lib/issues/998))
- `Contract.Keys` module that exposes smart constructors for `PublicKey` & `Ed25519Signature`, namely: `mkEd25519Signature`, `mkPubKey`.
- `Contract.createAdditionalUtxos` to build an expected utxo set from transaction outputs, useful for transaction chaining ([#1046](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1046))
- `DecodeAeson` instance for `NativeScript` data type ([#1069](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1069)).
- `Contract.Wallet` exports `mkWalletBySpec` ([#1157](https://github.com/Plutonomicon/cardano-transaction-lib/issues/1157))
- `ctl-server` NixOS module ([#1194](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1194)). See [nix/test-nixos-configuration.nix](nix/test-nixos-configuration.nix) for example usage and [nix/ctl-server-nixos-module.nix](nix/ctl-server-nixos-module.nix).
- Ability to run E2E tests on private Plutip testnets using CIP-30 wallet mock - see [the docs](./doc/e2e-testing.md#using-cip-30-mock-with-plutip)  ([#1166](https://github.com/Plutonomicon/cardano-transaction-lib/issues/1166))
- `Contract.Plutarch.Types` module with `PRational` type which is a newtype of Rational with `ToData` and `FromData` instance which are compatible with Plutarch ([#1221](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1221))
- New constraints for stake operations ([#1060](https://github.com/Plutonomicon/cardano-transaction-lib/issues/1060)):
  - Pool registration (`mustRegisterPool`)
  - Pool retirement (`mustRetirePool`)
  - Stake credential registration (`mustRegisterStakePubKey`, `mustRegisterStakeScript`)
  - Stake delegation (`mustDelegateStakeNativeScript`, `mustDelegateStakePlutusScript`, `mustDelegateStakePubKey`)
  - Staking rewards withdrawal (`mustWithdrawStakePubKey`, `mustWithdrawStakePlutusScript`, `mustWithdrawStakeNativeScript`)
  - Stake credential deregistration (`mustDeregisterStakePubKey`, `mustDeregisterStakePlutusScript`, `mustDeregisterStakeNativeScript`)
- New query layer functions to retrieve staking-related info from Ogmios ([#1060](https://github.com/Plutonomicon/cardano-transaction-lib/issues/1060)):
  - `Contract.Staking.getPoolIds`
  - `Contract.Staking.getPoolParameters`
  - `Contract.Staking.getPubKeyHashDelegationsAndRewards`
  - `Contract.Staking.getValidatorHashDelegationsAndRewards`
- `Contract.Test.Plutip.testPlutipContracts` to run multiple Mote Plutip tests on the same Plutip cluster, saving on cluster startup time. ([#1154](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1154)).
- `EncodeAeson` and `DecodeAeson` instances for `TransactionInput`, `TransactionOutput`/`TransactionOutputWithRefScript` and `PaymentPubKeyHash`([#1138](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1138))
- `mustSendChangeToAddress` balancer constraint, allowing to explicitly set the address to send all generated change to ([#1243](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1243))
- `mustUseUtxosAtAddress` and `mustUseUtxosAtAddresses` balancer constraints, allowing to specify addresses that should be treated like utxos sources during balancing ([#1243](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1243))
- Add ability to provide extra browser CLI arguments in E2E test suite ([#1253](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1253))

### Changed

- Bumped cardano-serialization-lib dependencies to version 11.1.1-alpha.1 ([#1163](https://github.com/Plutonomicon/cardano-transaction-lib/issues/1163))
- `Contract.Transaction.calculateMinFee` and `Contract.Transaction.calculateMinFeeM` now accept additional UTxOs.
- Reorganised the library into new namespaces. Namely: library internals, tests, and examples are now under `Ctl.Internal.*`, `Test.Ctl.*`, and `Ctl.Examples.*` respectively. Documentation and comments have been updated to use these new names, but not entries of previous releases in the changelog. ([#1039](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1039))
- Switched to `preview` testnet by default ([#1030](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1030))
- `addressFromBech32` checks that address network Id corresponds to the contract environment Id and is therefore lifted to the `Contract` monad ([#1062](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1062))
- `keyWalletPrivatePaymentKey` and `keyWalletPrivateStakeKey` are now in to the public `Contract.Wallet.Key` API. ([#1094](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1094))
- Completely new E2E (headless browser) test suite. It is now both easier to set up and use, and less flaky. The shell script has been removed (and re-implemented in PureScript). The suite can now be configured using environment variables (`test/e2e.env`) or CLI arguments. It is no more required to provide an accompanying tester script for each of the `Contract`s to be tested. ([#1058](https://github.com/Plutonomicon/cardano-transaction-lib/issues/1058), [#986](https://github.com/Plutonomicon/cardano-transaction-lib/issues/986))
- The `logLevel` from the config parameters is passed to the `customLogger` to allow consistent filtering of the logs. ([#1110](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1110)).
- Functions for working with `BigNum` are now in the public `Contract.Numeric.BigNum` API ([#1109](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1109)).
- `PublicKey` and `Ed25519Signature` types now wrap `RawBytes` instead of `Bech32String`.
- `TypeLevel.Nat`, needed to implement `HasPlutusSchema`, gets exported in `Contract.PlutusData` ([#1143](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1143)).
- `MintingPolicy` to an enum consisting of `PlutusScript` or `NativeScript` ([#1069](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1069))
- `Contract.Scripts` `applyArgs` is now monomorphic on the script parameter ([#1069](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1069))
- Adapted Gero wallet extension to `preview` network in E2E test suite ([#1086](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1086))
- `Contact.TextEnvelope` how provides more type safe interface with simplified error handling ([#988](https://github.com/Plutonomicon/cardano-transaction-lib/issues/988))
- Forbid minting zero tokens. ([#1156](https://github.com/Plutonomicon/cardano-transaction-lib/issues/1156))
- Modified functions `getWalletAddress`, `ownPubKeyHash`, `ownStakePubKeyHash`, `getWalletAddressWithNetworkTag` and `ownPaymentPubKeyHash` to return `Contract r (Array Adress)`.  ([#1045](https://github.com/Plutonomicon/cardano-transaction-lib/issues/1045))
- `pubKeyHashAddress` and `scriptHashAddress` now both accept an optional `Credential` that corresponds to the staking component of the address ([#1060](https://github.com/Plutonomicon/cardano-transaction-lib/issues/1060))
- `utxosAt` and `getUtxo` now use Kupo internally, `utxosAt` returns `UtxoMap` without `Maybe` context. The users will need to set `kupoConfig` in `ConfigParams`. ([#1185](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1185))
- `Interval` type is redesigned to restrain some finite intervals to be expressed in the system ([#1041](https://github.com/Plutonomicon/cardano-transaction-lib/issues/1041))

### Removed

- `balanceAndSignTxE`, `balanceAndSignTx`, `balanceAndSignTxs`, `balanceTxWithAddress`, `balanceTxsWithAddress`, `withBalancedAndSignedTx` and `withBalancedAndSignedTxs` from `Contract.Transaction` ([#1053](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1053))
- `ScriptOutput` is removed and therefore not exported by `Contract.Address` anymore. also, `Contract.Transaction` doesn't export `scriptOutputToTransactionOutput` anymore ([#652](https://github.com/Plutonomicon/cardano-transaction-lib/issues/652)).
- `Contract.TxConstraints.TxConstraint` type from public API. The users should rely on domain functions instead ([#1135](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1135))
- `Contract.Address.enterpriseAddressScriptHash`, `enterpriseAddressValidatorHash` - use `Contract.Address.addressPaymentValidatorHash` that works for base addresses as well, or `addressStakeValidatorHash` to get the stake component validator hash ([#1060](https://github.com/Plutonomicon/cardano-transaction-lib/issues/1060))
- `Contract.Address.enterpriseAddressMintingPolicyHash` and `enterpriseAddressStakeValidatorHash` - these functions didn't make much sense (too specific, a very rare use case). ([#1060](https://github.com/Plutonomicon/cardano-transaction-lib/issues/1060))
- `ScriptHash` type from the `Contract` interface (use `ValidatorHash`) ([#1060](https://github.com/Plutonomicon/cardano-transaction-lib/issues/1060))
- `Contract.Address` re-exports from `Contract.Scripts` ([#1060](https://github.com/Plutonomicon/cardano-transaction-lib/issues/1060))
- `Contract.Address.ownPubKeyHash` and `ownPubKeyHashes` - these are not needed, use `ownPaymentPubKeyHash` / `ownPaymentPubKeyHashes` ([#1211](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1211))
- `mustBalanceTxWithAddress` and `mustBalanceTxWithAddresses` balancer constraints - use a combination of `mustUseUtxosAtAddresses` and `mustSendChangeToAddress` to get the same behaviour ([#1243](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1243))
### Fixed

- Fix absence of `getUtxos` method in CIP-30 mock ([#1026](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1026))
- `awaitTxConfirmedWithTimeout` not respecting its timeout ([#1021](https://github.com/Plutonomicon/cardano-transaction-lib/issues/1021))
- Absence of `serializeData` in Plutip ([#1078](https://github.com/Plutonomicon/cardano-transaction-lib/issues/1078))
- Fix excessive reconnection attempts after `Contract` runtime finalization ([#965](https://github.com/Plutonomicon/cardano-transaction-lib/pull/965))
- Fix wallet extension error terminating the whole test suite ([#1209](https://github.com/Plutonomicon/cardano-transaction-lib/issues/1209))
- Now we can process multiple time constraints ([#1124](https://github.com/Plutonomicon/cardano-transaction-lib/issues/1124))
- E2E test suite didn't apply settings archive CLI option properly ([#1254](https://github.com/Plutonomicon/cardano-transaction-lib/issues/1254))

## [2.0.0] - 2022-09-12

### Added

- Plutip integration to run `Contract`s in local, private testnets ([#470](https://github.com/Plutonomicon/cardano-transaction-lib/pull/470))
- Ability to run `Contract`s in Plutip environment in parallel - `Contract.Test.Plutip.withPlutipContractEnv` ([#800](https://github.com/Plutonomicon/cardano-transaction-lib/issues/800))
- `withKeyWallet` utility that allows to simulate multiple actors in Plutip environment ([#663](https://github.com/Plutonomicon/cardano-transaction-lib/issues/663))
- `withStakeKey` utility that allows providing a stake key to be used by `KeyWallet`s in Plutip environment ([#838](https://github.com/Plutonomicon/cardano-transaction-lib/pull/838))
- `Alt` and `Plus` instances for `Contract`.
- `Contract.Utxos.getUtxo` call to get a single utxo at a given output reference
- `Contract.Monad.withContractEnv` function  that constructs and finalizes a contract environment that is usable inside a bracket callback. **This is the intended way to run multiple contracts**. ([#731](https://github.com/Plutonomicon/cardano-transaction-lib/pull/731))
- `Contract.Monad.stopContractEnv` function to finalize a contract environment (close the `WebSockets`). It should be used together with `mkContractEnv`, and is not needed with `withContractEnv`. ([#731](https://github.com/Plutonomicon/cardano-transaction-lib/pull/731))
- `Contract.Config` module that contains everything needed to create and manipulate `ConfigParams`, as well as a number of `ConfigParams` fixtures for common use cases. ([#731](https://github.com/Plutonomicon/cardano-transaction-lib/pull/731))
- `Contract.Monad.askConfig` and `Contract.Monad.asksConfig` functions to access user-defined configurations. ([#731](https://github.com/Plutonomicon/cardano-transaction-lib/pull/731))
- `Contract.Config.WalletSpec` type that allows to define wallet parameters declaratively in `ConfigParams`, instead of initializing wallet and setting it to a `ContractConfig` ([#731](https://github.com/Plutonomicon/cardano-transaction-lib/pull/731))
- Faster initialization of `Contract` runtime due to parallelism. ([#731](https://github.com/Plutonomicon/cardano-transaction-lib/pull/731))
- `purescriptProject`'s `shell` parameter now accepts `packageLockOnly`, which if set to true will stop npm from generating `node_modules`. This is enabled for CTL developers
- `Contract.Transaction.awaitTxConfirmed` and `Contract.Transaction.awaitTxConfirmedWithTimeout`
- `Contract.TextEnvelope.textEnvelopeBytes` and family to decode the `TextEnvelope` format, a common format output by tools like `cardano-cli` to serialize values such as cryptographical keys and on-chain scripts
- `Contract.Wallet.isNamiAvailable` and `Contract.Wallet.isGeroAvailable` functions ([#558](https://github.com/Plutonomicon/cardano-transaction-lib/issues/558)])
- `Contract.Transaction.balanceTxWithOwnAddress` and `Contract.Transaction.balanceTxsWithOwnAddress` to override an `Address` used in `balanceTx` internally ([#775](https://github.com/Plutonomicon/cardano-transaction-lib/pull/775))
- `Contract.Transaction.awaitTxConfirmedWithTimeoutSlots` waits a specified number of slots for a transaction to succeed. ([#790](https://github.com/Plutonomicon/cardano-transaction-lib/pull/790))
- `Contract.Transaction.submitE` like submit but uses an `Either (Array Aeson) TransactionHash` to handle a SubmitFail response from Ogmios
- `Contract.Chain.waitNSlots`,  `Contract.Chain.currentSlot` and `Contract.Chain.currentTime` a function to wait at least `N` number of slots and functions to get the current time in `Slot` or `POSIXTime`. ([#740](https://github.com/Plutonomicon/cardano-transaction-lib/issues/740))
- `Contract.Transaction.getTxByHash` to retrieve contents of an on-chain transaction.
- `project.launchSearchablePursDocs` to create an `apps` output for serving Pursuit documentation locally ([#816](https://github.com/Plutonomicon/cardano-transaction-lib/issues/816))
- `Contract.PlutusData.IsData` type class (`ToData` + `FromData`) ([#809](https://github.com/Plutonomicon/cardano-transaction-lib/pull/809))
- A check for port availability before Plutip runtime initialization attempt ([#837](https://github.com/Plutonomicon/cardano-transaction-lib/issues/837))
- `Contract.Address.addressToBech32` and `Contract.Address.addressWithNetworkTagToBech32` ([#846](https://github.com/Plutonomicon/cardano-transaction-lib/issues/846))
- `doc/e2e-testing.md` describes the process of E2E testing. ([#814](https://github.com/Plutonomicon/cardano-transaction-lib/pull/814))
- Added unzip to the `devShell`. New `purescriptProject.shell` flag `withChromium` also optionally adds Chromium to the `devShell` ([#799](https://github.com/Plutonomicon/cardano-transaction-lib/pull/799))
- Added paymentKey and stakeKey fields to the record in KeyWallet
- Added `formatPaymentKey` and `formatStakeKey` to `Wallet.KeyFile` and `Contract.Wallet` for formatting private keys
- Added `privatePaymentKeyToFile` and `privateStakeKeyToFile` to `Wallet.KeyFile` and `Contract.Wallet.KeyFile` for writing keys to files
- Added `bytesFromPrivateKey` to `Serialization`
- Improved error handling of transaction evaluation through Ogmios. This helps with debugging during balancing, as it requires the transaction to be evaluated to calculate fees. ([#832](https://github.com/Plutonomicon/cardano-transaction-lib/pull/832))
- `Contract.Hashing.transactionHash` to calculate the hash of the transaction ([#870](https://github.com/Plutonomicon/cardano-transaction-lib/pull/870))
- Flint wallet support ([#556](https://github.com/Plutonomicon/cardano-transaction-lib/issues/556))
- Support for `NativeScript`s in constraints interface: `mustPayToNativeScript` and `mustSpendNativeScriptOutput` functions ([#869](https://github.com/Plutonomicon/cardano-transaction-lib/pull/869))
- `Contract.Test.Cip30Mock` module to mock CIP-30 wallet interface using `KeyWallet`. The mock can be used for testing without a wallet (even in NodeJS environment). This increases test coverage for CTL code. ([#784](https://github.com/Plutonomicon/cardano-transaction-lib/issues/784))
- `Plutus.Types.AssocMap.AssocMap` now has `TraversableWithIndex`,  `FoldableWithIndex`,  `FunctorWithIndex` instances ([#943](https://github.com/Plutonomicon/cardano-transaction-lib/pull/943))
- The return value of `purescriptProject` now includes the project with its compiled `output` and its generated `node_modules` (under the `compiled` and `nodeModules` attributes, respectively) ([#956](https://github.com/Plutonomicon/cardano-transaction-lib/pull/956))
- `Contract.Utxos.getWalletUtxos` function that calls CIP-30 `getUtxos` method. ([#961](https://github.com/Plutonomicon/cardano-transaction-lib/issues/961))
- Lode wallet support ([#556](https://github.com/Plutonomicon/cardano-transaction-lib/issues/556))
- Added `Contract.Transaction.lookupTxHash` helper function ([#957](https://github.com/Plutonomicon/cardano-transaction-lib/issues/957))
- `Contract.Test.Utils` for making assertions about `Contract`s. ([#1005](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1005))
- `Examples.ContractTestUtils` demonstrating the use of `Contract.Test.Utils`. ([#1005](https://github.com/Plutonomicon/cardano-transaction-lib/pull/1005))
- `mustNotBeValid` constraint which marks the transaction as invalid, allowing scripts to fail during balancing and for Ogmios to allow submission. ([#947](https://github.com/Plutonomicon/cardano-transaction-lib/pull/947))
- Constraints for creating outputs with reference scripts: `mustPayToScriptWithScriptRef`, `mustPayToPubKeyAddressWithDatumAndScriptRef`, `mustPayToPubKeyAddressWithScriptRef`, `mustPayToPubKeyWithDatumAndScriptRef`, `mustPayToPubKeyWithScriptRef` ([#946](https://github.com/Plutonomicon/cardano-transaction-lib/pull/946))
- Constraints for using reference validators and minting policies: `mustSpendScriptOutputUsingScriptRef`, `mustMintCurrencyUsingScriptRef`, `mustMintCurrencyWithRedeemerUsingScriptRef` ([#946](https://github.com/Plutonomicon/cardano-transaction-lib/pull/946))
- Constraint for attaching a reference input to a transaction: `mustReferenceOutput` ([#946](https://github.com/Plutonomicon/cardano-transaction-lib/pull/946))
- `DatumPresence` data type, which tags paying constraints that accept datum, to mark whether the datum should be inline or hashed in the transaction output. ([#931](https://github.com/Plutonomicon/cardano-transaction-lib/pull/931))
- Utility conversion functions `serializeData` and `deserializeData` between `PlutusData` and `CborBytes` to `Contract.PlutusData`. ([#1001](https://github.com/Plutonomicon/cardano-transaction-lib/issues/1001))
- Added [CIP-30](https://cips.cardano.org/cips/cip30/) methods: `getNetworkId`, `getChangeAddress`, `getRewardAddresses`, `getUnusedAddresses`, `signData`, `isWalletAvailable`, `isEnabled`, `apiVersion`, `name` and `icon` to `Contract.Wallet` ([#974](https://github.com/Plutonomicon/cardano-transaction-lib/issues/974))

### Changed

- `PlutusScript` is now aware of which version of Plutus the script is for. The JSON representation has thus changed to reflect this and is not compatible with older JSON format.
- CTL's `overlay` no longer requires an explicitly passed `system`
- Switched to CSL for utxo min ada value calculation ([#715](https://github.com/Plutonomicon/cardano-transaction-lib/pull/715))
- Upgraded Haskell server to fully support Babbage-era transactions ([#733](https://github.com/Plutonomicon/cardano-transaction-lib/pull/733))
- Improved the collateral selection algorithm for `KeyWallet` ([#707](https://github.com/Plutonomicon/cardano-transaction-lib/pull/707))
- Switched to CSL for `PlutusScript` hashing ([#852](https://github.com/Plutonomicon/cardano-transaction-lib/pull/852))
- `runContract` now accepts `ConfigParams` instead of `ContractConfig` ([#731](https://github.com/Plutonomicon/cardano-transaction-lib/pull/731))
- `mkContractConfig` has been renamed to `mkContractEnv`. Users are advised to use `withContractEnv` instead to ensure proper finalization of WebSocket connections. ([#731](https://github.com/Plutonomicon/cardano-transaction-lib/pull/731))
- `ConfigParams` is now a type synonym instead of a newtype. `ContractConfig` has been renamed to `ContractEnv`.
- Moved logging functions to `Contract.Log` from `Contract.Monad` ([#727](https://github.com/Plutonomicon/cardano-transaction-lib/issues/727)
- Renamed `Contract.Wallet.mkKeyWalletFromPrivateKey` to `Contract.Wallet.mkKeyWalletFromPrivateKeys`.
- ServerConfig accepts a url `path` field ([#728](https://github.com/Plutonomicon/cardano-transaction-lib/issues/728)).
- Examples now wait for transactions to be confirmed and log success ([#739](https://github.com/Plutonomicon/cardano-transaction-lib/issues/739)).
- Updated CSL version to v11.0.0 ([#801](https://github.com/Plutonomicon/cardano-transaction-lib/issues/801))
- Better error message when attempting to initialize a wallet in NodeJS environment ([#778](https://github.com/Plutonomicon/cardano-transaction-lib/issues/778))
- The [`ctl-scaffold`](https://github.com/mlabs-haskell/ctl-scaffold) repository has been archived and deprecated and its contents moved to `templates.ctl-scaffold` in the CTL flake ([#760](https://github.com/Plutonomicon/cardano-transaction-lib/issues/760)).
- The CTL `overlay` output has been deprecated and replaced by `overlays.purescript`, `overlays.runtime`, and `overlays.ctl-server` ([#796](https://github.com/Plutonomicon/cardano-transaction-lib/issues/796) and [#872](https://github.com/Plutonomicon/cardano-transaction-lib/issues/872)).
- `buildCtlRuntime` and `launchCtlRuntime` now take an `extraServices` argument to add `docker-compose` services to the resulting Arion expression ([#769](https://github.com/Plutonomicon/cardano-transaction-lib/issues/769)).
- Use `cardano-serialization-lib` for fee calculation, instead of server-side code.
- `balanceAndSignTx` no longer silently drops error information via `Maybe`. The `Maybe` wrapper is currently maintained for API compatibility, but will be dropped in the future.
- Made it impossible to write unlawful `EncodeAeson` instances ([#490](https://github.com/Plutonomicon/cardano-transaction-lib/issues/490))
- The `ctl-server` component of the runtime is now optional and is only required when using the `applyArgs` endpoint ([#872](https://github.com/Plutonomicon/cardano-transaction-lib/issues/872)). Related changes include:
  - The `ctlServerConfig` fields of both `ConfigParams` and `PlutipConfig` now take a `Maybe ServerConfig`. In the case of `PlutipConfig`, a `Just` value will spawn the service inside the Plutip test. For the `ConfigParams` type, calls to `applyArgs` will fail when the field is set to `Nothing`.
  - The config accepted by `launchCtlRuntime` and `buildCtlRuntime` now takes a `ctl-server.enable` field. If `false`, `ctl-server` will not be launched.
- `SlotLength` and `RelativeTime` in `EraSummary` from Ogmios are now of type `Number` instead of `BigInt`. Also add `Maybe` around some functions in `Type.Interval` or changed it's signature to use `Number`. ([#868](https://github.com/Plutonomicon/cardano-transaction-lib/issues/868))
- The `ProtocolParameters` introduced in Alonzo (`prices`, `maxTxExUnits`, `maxBlockExUnits`, `maxValueSize`, `collateralPercent` and `maxCollateralInputs`) are no longer of type `Maybe` because we don't support pre-Alonzo eras. ([#971](https://github.com/Plutonomicon/cardano-transaction-lib/issues/971))
- Renamed `UtxoM` to `UtxoMap` ([#963](https://github.com/Plutonomicon/cardano-transaction-lib/pull/963))
- KeyWallet's `selectCollateral` field now allows multiple collateral to be selected, and is provided with `coinsPerUtxoByte` and `maxCollateralInputs` from the protocol parameters. ([#947](https://github.com/Plutonomicon/cardano-transaction-lib/pull/947))
- `mustPayWithDatumToPubKey`, `mustPayWithDatumToPubKeyAddress`, and `mustPayToScript` now expect a `DatumPresence` tag in their arguments to mark whether the datum should be inline or hashed in the transaction output. ((#931)[https://github.com/Plutonomicon/cardano-transaction-lib/pull/931))
- Switched to [blakejs](https://github.com/dcposch/blakejs) for blake2b hashing. `blake2b256Hash` and `blake2b256HashHex` functions are now pure ([#991](https://github.com/Plutonomicon/cardano-transaction-lib/pull/991))
- Updated ODC version, this includes a new function `getDatumsByHashesWithErrors`that does not discard errors, unlike `getDatumsByHashes`. **This update also changes the way we store transactions in the local database, meaning that we need to drop the `transactions` table.**

### Removed

- `Contract.Monad.traceTestnetContractConfig` - use `Contract.Config.testnetNamiConfig` instead (or other variants of `testnet...Config` for other wallets).
- `runContract_` - use `void <<< runContract`.
- `Contract.Aeson` module - use `Aeson` ([#938](https://github.com/Plutonomicon/cardano-transaction-lib/issues/938))

### Fixed

- Endless `awaitTxConfirmed` calls ([#804](https://github.com/Plutonomicon/cardano-transaction-lib/issues/804))
- Bug with collateral selection: only the first UTxO provided by wallet was included as collateral [(#723)](https://github.com/Plutonomicon/cardano-transaction-lib/issues/723)
- Bug with collateral selection for `KeyWallet` when signing multiple transactions ([#709](https://github.com/Plutonomicon/cardano-transaction-lib/pull/709))
- Bug when zero-valued non-Ada assets were added to the non-Ada change output ([#802](https://github.com/Plutonomicon/cardano-transaction-lib/pull/802))
- Error recovery logic for `SubmitTx` if the WebSocket connection is dropped ([#870](https://github.com/Plutonomicon/cardano-transaction-lib/pull/870))
- Properly implemented CIP-25 V2 metadata. Now there's no need to split arbitrary-length strings manually to fit them in 64 PlutusData bytes (CTL handles that). A new `Cip25String` type has been introduced (a smart constructor ensures that byte representation fits 64 bytes, as required by the spec). Additionally, a new `Metadata.Cip25.Common.Cip25TokenName` wrapper over `TokenName` is added to ensure proper encoding of `asset_name`s. There are still some minor differences from the spec:
  - We do not split strings in pieces when encoding to JSON
  - We require a `"version": 2` tag
  - `policy_id` must be 28 bytes
  - `asset_name` is up to 32 bytes. See https://github.com/cardano-foundation/CIPs/issues/303 for motivation
- `ogmios-datum-cache` now works on `x86_64-darwin`
- `TypedValidator` interface ([#808](https://github.com/Plutonomicon/cardano-transaction-lib/issues/808))
- `Contract.Address.getWalletCollateral` now works with `KeyWallet`.
- Removed unwanted error messages in case `WebSocket` listeners get cancelled ([#827](https://github.com/Plutonomicon/cardano-transaction-lib/issues/827))
- Bug in `CostModel` serialization - incorrect `Int` type ([#874](https://github.com/Plutonomicon/cardano-transaction-lib/issues/874))
- Use logger settings on Contract initialization ([#897](https://github.com/Plutonomicon/cardano-transaction-lib/issues/897))
- Disallow specifying less than 1 ADA in Plutip UTxO distribution ([#901](https://github.com/Plutonomicon/cardano-transaction-lib/pull/901))
- Bug in `TransactionMetadatum` deserialization ([#932](https://github.com/Plutonomicon/cardano-transaction-lib/issues/932))
- Fix excessive logging after the end of `Contract` execution ([#893](https://github.com/Plutonomicon/cardano-transaction-lib/issues/893))
- Add ability to suppress logs of successful `Contract` executions - with new `suppressLogs` config option the logs will be shown on error ([#768](https://github.com/Plutonomicon/cardano-transaction-lib/issues/768))
- Fix `runPlutipTest` not passing custom `buildInputs` ([#955](https://github.com/Plutonomicon/cardano-transaction-lib/pull/954))
- Problem parsing ogmios `SlotLength` and `RelativeTime` in era Summaries if those include non integer values. ([#906](https://github.com/Plutonomicon/cardano-transaction-lib/pull/906))
- Use `docs-search-0.0.12` that properly lists modules consisting only of re-exports ([#973](https://github.com/Plutonomicon/cardano-transaction-lib/issues/973))
- Inline datum in Ogmios transaction outputs are now parsed and preserved when converting to CTLs respective type. ([#931](https://github.com/Plutonomicon/cardano-transaction-lib/pull/931))

## [2.0.0-alpha] - 2022-07-05

This release adds support for running CTL contracts against Babbage-era nodes. **Note**: this release does not support Babbagge-era features and improvements, e.g. inline datums and reference inputs. Those feature will be implemented in `v2.0.0` proper.

### Added

- Support for using a `PrivateKey` as a `Wallet`.
- `mkKeyWalletFromFile` helper to use `cardano-cli`-style `skey`s
- Single `Plutus.Conversion` module exposing all `(Type <-> Plutus Type)` conversion functions ([#464](https://github.com/Plutonomicon/cardano-transaction-lib/pull/464))
- `logAeson` family of functions to be able to log JSON representations
- `EncodeAeson` instances for most types under `Cardano.Types.*` as well as other useful types (`Value`, `Coin`, etc.)
- `getProtocolParameters` call to retrieve current protocol parameters from Ogmios ([#541](https://github.com/Plutonomicon/cardano-transaction-lib/issues/541))
- `Contract.Utxos.getWalletBalance` call to get all available assets as a single `Value` ([#590](https://github.com/Plutonomicon/cardano-transaction-lib/issues/590))
- `balanceAndSignTxs` balances and signs multiple transactions while taking care to use transaction inputs only once
- Ability to load stake keys from files when using `KeyWallet` ([#635](https://github.com/Plutonomicon/cardano-transaction-lib/issues/635))
- Implement utxosAt for `KeyWallet` ([#617](https://github.com/Plutonomicon/cardano-transaction-lib/issues/617))
- `FromMetadata` and `ToMetadata` instances for `Contract.Value.CurrencySymbol`
- `Contract.Chain.waitUntilSlot` to delay contract execution until local chain tip reaches certain point of time (in slots).

### Removed

- `FromPlutusType` / `ToPlutusType` type classes. ([#464](https://github.com/Plutonomicon/cardano-transaction-lib/pull/464))
- `Contract.Wallet.mkGeroWallet` and `Contract.Wallet.mkNamiWallet` - `Aff` versions should be used instead
- Protocol param update setters for the decentralisation constant (`set_d`) and the extra entropy (`set_extra_entropy`) ([#609](https://github.com/Plutonomicon/cardano-transaction-lib/pull/609))
- `AbsSlot` and related functions have been removed in favour of `Slot`
- Modules `Metadata.Seabug` and `Metadata.Seabug.Share`
- `POST /eval-ex-units` Haskell server endpoint ([#665](https://github.com/Plutonomicon/cardano-transaction-lib/pull/665))
- Truncated test fixtures for time/slots inside `AffInterface` to test time/slots not too far into the future which can be problematic during hardforks https://github.com/Plutonomicon/cardano-transaction-lib/pull/676
- `d` and `extraEntropy` protocol parameters from protocol parameters update proposal

### Changed

- Updated `ogmios-datum-cache` - bug fixes ([#542](https://github.com/Plutonomicon/cardano-transaction-lib/pull/542), [#526](https://github.com/Plutonomicon/cardano-transaction-lib/pull/526), [#589](https://github.com/Plutonomicon/cardano-transaction-lib/pull/589))
- Improved error response handling for Ogmios ([#584](https://github.com/Plutonomicon/cardano-transaction-lib/pull/584))
- `balanceAndSignTx` now locks transaction inputs within the current `Contract` context. If the resulting transaction is never used, then the inputs must be freed with `unlockTransactionInputs`.
- Updated `ogmios-datum-cache` - bug fixes (#542, #526, #589).
- Made protocol parameters part of `QueryConfig`.
- Refactored `Plutus.Conversion.Address` code (utilized CSL functionality).
- Changed the underlying type of `Slot`, `TransactionIndex` and `CertificateIndex` to `BigNum`.
- Moved transaction finalization logic to `balanceTx`.
- Upgraded to CSL v11.0.0-beta.1.
- `purescriptProject` (exposed via the CTL overlay) was reworked significantly. Please see the [updated example](https://github.com/Plutonomicon/cardano-transaction-lib/blob/develop/doc/ctl-as-dependency.md#using-ctls-overlay) in the documentation for more details.
- Switched to Ogmios for execution units evaluation ([#665](https://github.com/Plutonomicon/cardano-transaction-lib/pull/665))
- Changed `inputs` inside `TxBody` to be `Set TransactionInput` instead `Array TransactionInput`. This guarantees ordering of inputs inline with Cardano ([#641](https://github.com/Plutonomicon/cardano-transaction-lib/pull/661))
- Upgraded to Ogmios v5.5.0
- Change `inputs` inside `TxBody` to be `Set TransactionInput` instead `Array TransactionInput`. This guarantees ordering of inputs inline with Cardano ([#641](https://github.com/Plutonomicon/cardano-transaction-lib/pull/661)).

### Fixed

- Handling of invalid UTF8 byte sequences in the Aeson instance for `TokenName`
- `Types.ScriptLookups.require` function naming caused problems with WebPack ([#593](https://github.com/Plutonomicon/cardano-transaction-lib/pull/593))
- Bad logging in `queryDispatch` that didn't propagate error messages ([#615](https://github.com/Plutonomicon/cardano-transaction-lib/pull/615))
- Utxo min ada value calculation ([#611](https://github.com/Plutonomicon/cardano-transaction-lib/pull/611))
- Discarding invalid inputs in `txInsValues` instead of yielding an error ([#696](https://github.com/Plutonomicon/cardano-transaction-lib/pull/696))
- Locking transaction inputs before the actual balancing of the transaction ([#696](https://github.com/Plutonomicon/cardano-transaction-lib/pull/696))

## [1.1.0] - 2022-06-30

### Fixed

- Changed `utxoIndex` inside an `UnbalancedTx` to be a `Map` with values `TransactionOutput` instead of `ScriptOutput` so there is no conversion in the balancer to `ScriptOutput`. This means the balancer can spend UTxOs from different wallets instead of just the current wallet and script addresses.

## [1.0.1] - 2022-06-17

### Fixed

- `mustBeSignedBy` now sets the `Ed25519KeyHash` corresponding to the provided `PaymentPubKeyHash` directly. Previously, this constraint would fail as there was no way to provide a matching `PaymentPubKey` as a lookup. Note that this diverges from Plutus as the `paymentPubKey` lookup is always required in that implementation.

## [1.0.0] - 2022-06-10

CTL's initial release!
