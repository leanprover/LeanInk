#!/bin/bash
git clone https://github.com/insightmind/LeanInk ./leanInk -q
cd leanInk
git fetch --tags -q
latestTag=$(git describe --tags `git rev-list --tags --max-count=1`)
git checkout $latestTag -q
sh install.sh
cd ..
rm -rf ./leanInk