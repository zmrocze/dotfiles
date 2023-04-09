#!/bin/bash

echo "inits database in db directory"

mkdir db

chown -R postgres:postgres db/data

doas -u postgres -- initdb -D db/data

# cp 


