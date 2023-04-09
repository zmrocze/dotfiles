#!/bin/bash

echo "inits database in db directory"

USER=zmrocze

mkdir db

doas chown -R $USER:$USER db

doas -u $USER -- initdb -D db/data

# cp 


