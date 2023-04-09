#!/bin/bash

doas -u postgres -- pg_ctl start -D /home/zmrocze/code/haskell/cryptocurrency/db/data -l /home/zmrocze/code/haskell/cryptocurrency/db/logs/logfile0

doas -u postgres -- createdb --host=localhost --port=5432 wallet_db

doas -u postgres -- psql --host=localhost --port=5432 -d wallet_db -f schema/wallet.psql