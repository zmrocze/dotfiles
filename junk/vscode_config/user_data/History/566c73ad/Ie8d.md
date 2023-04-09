@koslambrou  I think its ready for review.

What is left after that pr:
 - Add constraints in `plutus-ledger-constraints` concerning `Withdrawals`, `Certificate`, `Metadata` (functional constraint)
 - Swap `DCert` in the `txCertificates` field of `Tx` for i.e. cardano-api/`Certificate`.
 - `TODO: Implement ToSchema TxMetadataValue`
 - `TODO: Parse certifying redeemer.`, `TODO: Parse rewarding redeemer.`, `TODO: ToJSON rewarding/certifying Redeemer.`

Though I think its right to review it already as by its nature it modifies code all over the place and at merges there is a flood of nontrivial merge conflicts.