#!/usr/bin/env bash

ulimit -s 8192
DIFF=diff
if diff --color --help >/dev/null 2>&1; then
    DIFF="diff --color";
fi

_realpath () (
  OURPWD=$PWD
  cd "$(dirname "$1")"
  LINK=$(readlink "$(basename "$1")")
  while [ "$LINK" ]; do
    cd "$(dirname "$LINK")"
    LINK=$(readlink "$(basename "$1")")
  done
  REALPATH="$PWD/$(basename "$1")"
  cd "$OURPWD"
  echo "$REALPATH"
)

REALPATH=realpath

if ! command -v realpath &> /dev/null; then
    REALPATH=_realpath
fi

LEANINK=$($REALPATH "../build/bin/leanInk")

error () {
    echo $1
    exit 1
}

build_leanink () {
    lake build || error "Failed to compile leanInk"zsh -df
}

run_leanink () {
    file=$1
    directory=${file%/*}
    filename=${file##*/}
    # If the folder in which the file resides contains a lakefile we will use it to resolve any dependencies for the file
    if [[ -f "$directory/lakefile.lean" ]]; then
        echo "Running LeanInk with lakefile.lean - $file"
        (cd $directory && $LEANINK analyze $filename --x-enable-type-info --x-enable-docStrings --x-enable-semantic-token --lake lakefile.lean --verbose) || error "LeanInk failed - $file"
    else
        echo "Running LeanInk - $file"
        (cd $directory && $LEANINK analyze $filename --x-enable-type-info --x-enable-docStrings --x-enable-semantic-token --verbose) || error "LeanInk failed - $file"
    fi
}

run_tests () {
    echo "Running diff tests for leanInk"
    retVal=0
    for file in $(find . -name '*.lean'); do
        if [[ ${file##*/} != "lakefile.lean" && $file != *"/lean_packages/"* ]]; then 
            if test "$file.leanInk.expected"; then
                run_leanink $file
                if $DIFF -au --strip-trailing-cr "$file.leanInk.expected" "$file.leanInk"; then
                    echo "SUCCESSFULL! ($file)"
                else
                    echo "FAILED! ($file)"
                    retVal=1
                fi
            else
                error "No expected output file found: $file.leanInk.expected"
            fi
        fi
    done

    exit $retVal
}

run_capture () {
    echo "Create new expected output files for leanInk tests. Overriding previous output files!"
    for file in $(find . -name '*.lean'); do
        if [[ ${file##*/} != "lakefile.lean" ]] && [[ $file != *"/lean_packages/"* ]]; then 
            run_leanink $file
            mv "$file.leanInk" "$file.leanInk.expected"
            echo "Generated $file.leanInk.expected"
        fi
    done
}

# Allows to call a function based on arguments passed to the script
$*
