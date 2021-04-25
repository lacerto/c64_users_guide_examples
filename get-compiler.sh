#!/bin/sh

bin_dir=${1:-./bin}
os=$(uname)
compiler="$bin_dir/$os/tmpx"

if [ -x "$compiler" ]; then
    echo $compiler
else
    echo $(which false)
fi
