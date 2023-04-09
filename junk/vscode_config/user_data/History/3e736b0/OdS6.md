Hey, I have two questions:

1. I want to bump one dependency version (plutus-0.1.0.0 -> plutus-1.0.0.0). Project depends on cardano-wallet. There is no existing branch for wallet that depends on plutus 1.0.0.0. Will this result in dependency conflict always or no and I can use different versions of a lib for different components?

2. Generally: How one goes about bumping dependency versions? it seems that I'm solving np-complete task instead of letting computer do it. I understand that versions are specified in cabal.project , by listing pinned commits - and then provided by nix. But this way I can't ask cabal to find any package versions working with plutus-1.0.0.0 as they are all fixed to a given commit. Cabal would have to search through all commits on a given repo, can it? (still this wouldn't have solved 1.)
  a) Cabal files (cabal.project and local project/project.cabal) are only sources of dependency constraints, correct?
  b) 
