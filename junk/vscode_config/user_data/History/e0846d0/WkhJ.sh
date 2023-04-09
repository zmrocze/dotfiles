
for test in '/renders a vesting scenario sensibly/' '/renders the log of a single contract instance sensibly/' '/renders the emulator log sensibly/' '/renders an error sensibly/'
do 
    cabal run plutus-use-cases-test -- -p "$test"
done 


        
        "test/Spec/crowdfundingWallet1TestOutput.txt"

        "renders the emulator log sensibly"
        "test/Spec/crowdfundingEmulatorTestOutput.txt"
        "renders an error sensibly"
