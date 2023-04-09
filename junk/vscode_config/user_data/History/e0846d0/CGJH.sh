
for test in '/renders the log of a single contract instance sensibly/' '/renders the emulator log sensibly/' '/renders an error sensibly/' '/renders a crowdfunding scenario sensibly/' '/renders a game guess scenario sensibly/' '/renders a vesting scenario sensibly/'
do 
    cabal run plutus-use-cases-test -- -p "$test"
done 