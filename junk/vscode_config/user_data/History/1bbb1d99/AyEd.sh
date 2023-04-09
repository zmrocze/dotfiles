#!/bin/bash

echo "inits database in db directory"

USER=zmrocze

mkdir db

doas chown -R postgres:postgres db

doas -u postgres -- initdb -D db/data

# cp 


