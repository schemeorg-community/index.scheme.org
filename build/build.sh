#!/bin/bash

ROOT=$(cd ../ && pwd)
echo "Project root $ROOT"

echo 'Building Chicken scheme scripts'
cd $ROOT/scheme-index-util
csc buildtypes.scm
csc buildfilters.scm

echo 'Converting definitions to json'
cd $ROOT
./scheme-index-util/buildtypes types/index.scm scheme-index-app/src/assets/types.json
./scheme-index-util/buildfilters filters/index.scm scheme-index-app/src/assets/filters.json

echo 'Building scheme index app'
cd $ROOT/scheme-index-app/
npm install
npm run build

echo 'Zipping result'
cd $ROOT/scheme-index-app/
zip -r $ROOT/build/schemeindex.zip dist

echo "Build output at $ROOT/build/schemeindex.zip"
