#
latest cardano-node release 1.34.1 pins plutus 0.1.0.0 but main branch already pins 1.0.0.0 
plutus-apps pins 0.1.0.0 and says it should follow cardano-node

#
the new plutus-ledger-constraints uses Tx from plutus-0.1.0.0. It should probably be updated (ledger-constraints) to also care for withdrawals in constraints and so on.

# 
Signature, PubKey also missing

#
move-tx-from-plutus contains tx from plutus with commit history and unnesecary plutus dep bump.

# sth fishy
context: rarely is TxInType of form Just ...

in lkpOutputs in plutus-ledger/src/Ledger/Index.hs:260 the TxOutRef is not even compared with TxInType. 

^ but toCardanoApi errors if it is Nothing

# sth fishy 2
why does toCardanoTxInWitness hardcodes zeroExecutionCosts