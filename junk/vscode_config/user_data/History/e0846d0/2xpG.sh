
for test in '/renders a vesting scenario sensibly/' '/renders the log of a single contract instance sensibly/' '/renders the emulator log sensibly/' '/renders an error sensibly/' 
do 
    cabal run plutus-use-cases-test -- -p "$test"
done 

'/renders a crowdfunding scenario sensibly/' '/renders a game guess scenario sensibly/' '/renders a vesting scenario sensibly/'