- How to debug flake? I want inputs resolved and peak into what are the values of attributes 
 of outputs already applied to resolved inputs and system.

# mlabs-tooling
 - template doesn't show how to add any deps, it could

 - bug: extraHackage input overwritten by CHaP? (or no, because there's few plutus at chap) but doesn't pick up right plutus

 - cardano deps have poor version bounds (inherited from times where every dep was fixed to single commit outside of .cabal files)
 and this is not solved with mlabs-tooling 

 - short overview of how mlabs-tooling + cabal chooses the dependency set:
   - is a dependency picked from CHaP or extraHackage first?
   - are version bounds from cabal.project's taken into consideration?