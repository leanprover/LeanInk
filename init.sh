#!/bin/bash
git clone https://github.com/insightmind/LeanInk ./tmp -q
cd tmp
git fetch --tags -q
latestTag=$(git describe --tags `git rev-list --tags --max-count=1`)
git checkout $latestTag -q
cd ..
sh tmp/install.sh
rm -rf tmp