#!/bin/sh
for nc in `ls orig/????.nc` ; do
    new_file=`basename $nc`
    echo "$nc -> $new_file"
    ruby skip_data.rb $nc@temp -o $new_file -s 10,10,2 --copy-dims=ref_time
done