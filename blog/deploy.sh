#!/bin/bash
cp -r _site/* deploy/
pushd deploy
git add --all
git commit -m "snapshot $(date '+%y-%m-%d %H:%M')"
git push origin gh-pages
popd
