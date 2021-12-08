ulimit -s 8192
DIFF=diff
if diff --color --help >/dev/null 2>&1; then
    DIFF="diff --color";
fi

LEANINK="../build/bin/leanInk"

function error {
    echo $1
    exit 1
}

function build_leanink {
    lake build || error "Failed to compile leanInk"zsh -df
}

function run_leanink {
    file=$1
    directory=${file%/*}
    filename=${file##*/}
    # If the folder in which the file resides contains a lakefile we will use it to resolve any dependencies for the file
    if [[ -f "$directory/lakefile.lean" ]]; then
        echo "Running LeanInk with lakefile.lean - $file"
        (cd $directory && LEANINK analyze $filename --lake lakefile.lean || error "LeanInk failed - $file")
    else
        echo "Running LeanInk - $file"
        (cd $directory && LEANINK analyze $filename || error "LeanInk failed - $file")
    fi
}

function run_tests {
    echo "Running diff tests for leanInk"
    retVal=0
    for file in $(find . -name '*.lean'); do
        if [[ ${file##*/} != "lakefile.lean" ]] && [[ $file != *"/lean_packages/"* ]]; then 
            if test "$file.leanInk.expected"; then
                run_leanink $file
                if $DIFF -au --strip-trailing-cr "$file.leanInk" "$file.leanInk.expected"; then
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

function run_capture {
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