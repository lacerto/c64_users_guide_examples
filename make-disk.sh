#!/bin/sh

for prg_path in $1/*.prg; do
    prg_file=${prg_path##*/}
    prg_file_without_ext=${prg_file%.*}
	c1541 -attach $2 -delete $prg_file_without_ext
	c1541 -attach $2 -write $prg_path $prg_file_without_ext
done