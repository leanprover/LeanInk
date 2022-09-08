#!/bin/bash
git clone https://github.com/insightmind/LeanInk ./leanInk -q
cd leanInk
git fetch --tags -q
latestTag=$(git describe --tags `git rev-list --tags --max-count=1`)
git checkout $latestTag -q
lake script run install && cd .. && rm -rf ./leanInk