#!/bin/bash
file=$1
pid=$2
rm -f $file $file.DONE
while [ ! -f "$file.DONE" ] ; do
ps h -p $pid -o rss >> $file
done
