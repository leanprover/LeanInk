#!/bin/bash

error () {
    echo $1
}

echo "Compiling LeanInk..."
# First we have to build leanInk.
lake build > /dev/null || error "Failed to build leanInk. Make sure your lean install is correct!"

# As we can assume that lean is already installed as expected we can assume
# that the .elan folder already exists and is correctly linked.
ELAN_BIN="$HOME/.elan/bin/"

echo "Installing LeanInk..."
if [[ -f "$ELAN_BIN/leanInk" ]]; then
    OLD_LEAN_INK=$(eval "$ELAN_BIN/leanInk -v")
    NEW_LEAN_INK=$(eval "./build/bin/leanInk -v")
    echo "LeanInk ($OLD_LEAN_INK) already installed. Do you want to overwrite it with LeanInk ($NEW_LEAN_INK) [Y|n]?"
    read < /dev/stdin
    SHOULD_OVERWRITE="$(tr '[:upper:]' '[:lower:]' <<< "${REPLY}")"
    if [[ "${SHOULD_OVERWRITE}" == "y" ]]; then 
        cp -f ./build/bin/leanInk ~/.elan/bin/ || error "Failed copying LeanInk to .elan/bin!"
        echo "New LeanInk version successfully installed!"
    else
        echo "New LeanInk version not installed!"
    fi
else        
    if cp ./build/bin/leanInk "$ELAN_BIN"; then
        echo "LeanInk version successfully installed!"
    else
        echo "Failed copying LeanInk to .elan/bin!"
    fi
fi
