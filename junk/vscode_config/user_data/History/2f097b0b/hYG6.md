# mlabs-plutus-Template Roadmap

- [ ] **Stage 1** (_ hours)
  - onchain
  - [ ] Example onchain with AlwaysSucceeds and script builder
    - [ ] Plutus
    - [ ] Plutarch
  - offchain
    - [ ] End-to-end CTL contracts execution from the browser (on plutip network)
      List of endpoints to run listed contracts
      Actual browser or headless in terminal?
      - [ ] Submit script transaction example contract (wallet?)
  - tools
    - [ ] formatters, build, ply, hoogle
    - [ ] GH commit hooks
- [ ] **Stage 2** (_ hours)
  - onchain
    - [ ] plutus-simple-model test suite
    - [ ] Applied scripts examples
  - offchain
    - [ ] e2e contract execution test suite in the (headless?) browser
      - [ ] plutip
      - [ ] public testnet
  - tools 
    - [ ] CI executes all tests (public testnet?)
- [ ] **Stage 3** (_ hours)
  - frontend
    - [ ] React GUI for calling a script endpoint (ala plutus playground)
    - [ ] Configurable execution backend? 
      How to modular?
- [ ] **Stage 5**
  - offchain
    - [ ] deployment scripts
    - [ ] load balancer
      - Some example like server 