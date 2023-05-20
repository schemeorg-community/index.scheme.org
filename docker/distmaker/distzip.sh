#!/bin/sh
docker build -t scheme-index-tmp -f ../scheme-index/Dockerfile ../..
docker build -t scheme-index-nginx-tmp -f ../nginx/Dockerfile ../..
docker build -t scheme-index-dist -f Dockerfile .
docker create --name dummy scheme-index-dist
docker cp dummy:/dist.zip dist.zip
docker rm -f dummy
docker rmi scheme-index-dist scheme-index-tmp scheme-index-nginx-tmp
