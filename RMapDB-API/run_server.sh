#!/bin/bash

source env/bin/activate
export FLASK_APP=rmapdb
export FLASK_ENV=development
flask init-db
flask run
