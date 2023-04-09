
1. I want to bump one dependency version (plutus-0.1.0.0 -> plutus-1.0.0.0). Project depends on cardano-wallet. There is no existing branch for wallet that depends on plutus 1.0.0.0. Will this result in dependency conflict always or no and I can use different versions of a lib for different components?

2. Generally: How one goes about bumping dependency versions? it seems that I'm solving np-complete task instead of letting computer do it. 
I understand that versions are specified in cabal.project , by listing pinned commits - and then provided by nix.
