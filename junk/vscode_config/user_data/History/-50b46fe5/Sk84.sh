#!/bin/bash

USER=zmrocze

doas -u $USER -- pg_ctl start -D /home/zmrocze/code/haskell/cryptocurrency/db/data -l /home/zmrocze/code/haskell/cryptocurrency/db/logs/logfile0

doas -u $USER -- createdb --host=localhost --port=5432 wallet_db

doas -u $USER -- psql --host=localhost --port=5432 -d wallet_db -f schema/wallet.psql

mkdir db/logs

ADDCONFIGS="""

unix_socket_directories = '/home/zmrocze/code/haskell/cryptocurrency/db/run/postgresql' 	# karol
log_statement = 'all'			# none, ddl, mod, all  # karol
"""
echo "$ADDCONFIGS" >> db/data/postgresql.conf