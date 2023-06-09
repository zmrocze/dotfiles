# mlabs-plutus-Template Roadmap

- [ ] **Stage 1** (_ hours)
  - onchain
  - [ ] Example onchain with AlwaysSucceeds
    - [ ] Plutus
    - [ ] Plutarch
  - [ ] script builder producing json mapping and directory with CBOR scripts
  - offchain
    - [ ] CTL contracts execution from the browser (on plutip network),
      list of endpoints to run listed contracts
      (Actual browser or headless in terminal?)
      - [ ] Submit script transaction example contract (wallet?)
  - tools
    - [ ] formatters, build, ply, hoogle
    - [ ] GH commit hooks
- [ ] **Stage 2** (_ hours)
  - onchain
    - [ ] plutus-simple-model example test suite
    - [ ] Applied scripts examples, scripts dependant on previous script hashes
  - offchain
    - [ ] e2e contract execution test suite in the headless browser
      - [ ] plutip
      - [ ] public testnet
    - 
    - pain-free on-chain script importing
  - tools 
    - [ ] CI executes e2e plutip tests (public testnet?)
  - frontend
    - [ ] React GUI for calling a script endpoint (ala plutus playground)
    - [ ] Configurable execution backend?
- [ ] **Stage 3**
  - offchain
    - [ ] deployment scripts (links to the devops repo?)
      - [ ] example cluster deploy on _ cloud provider (terraform?)
        - [ ] on testnet
      - [ ] load balancer
      - [ ] metrics
      - [ ] hydra CD setup?
- [ ] **Stage 4**
  - offchain
    - [ ] custom indexers