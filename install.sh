#!/bin/bash

BASE=.
DEST=$BASE/root/cdoc

mkdir -p $DEST
mkdir -p $BASE/logs
ln -f chickadee-jquery.js $DEST
ln -f chickadee.css $DEST
ln -f jquery.metadata.min.js $DEST/jquery.metadata.2.1.min.js
ln -f mag.png $DEST

