#!/bin/bash

ROOT=$(cd ../ && pwd)
echo "Project root $ROOT"

echo 'Building Chicken scheme scripts'
cd $ROOT/chicken
csc builddb.scm
csc buildfilters.scm

echo 'Converting definitions to json'
cd $ROOT
./chicken/builddb types/index.scm client/frontend/src/assets/types.json
./chicken/buildfilters filters/index.scm client/frontend/src/assets/filters.json

echo 'Building frontend app'
cd $ROOT/client/frontend/
npm run build

echo 'Zipping result'
cd $ROOT/client/frontend/
zip -r $ROOT/build/schemeindex.zip dist

echo "Build output at $ROOT/build/schemeindex.zip"
