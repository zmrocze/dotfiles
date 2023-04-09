#!/bin/bash

echo "inits database in db directory"

USER=zmrocze

mkdir db

mkdir db/logs
mkdir -p db/run/postgresql

doas chown -R $USER:$USER db

doas -u $USER -- initdb -D db/data

ADDCONFIGS="""

unix_socket_directories = '/home/zmrocze/code/haskell/cryptocurrency/db/run/postgresql' 	# karol
log_statement = 'all'			# none, ddl, mod, all  # karol
"""
echo "$ADDCONFIGS" >> db/data/postgresql.conf
# cp 
