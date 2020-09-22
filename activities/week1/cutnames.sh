#!/bin/env bash
# Stats 506, Fall 2020
# Updated: Sep 10, 2020 by Yanyu Long
# 79: -------------------------------------------------------------------------
file=$1
pattern=$2

# a - download data if not present
# file="recs2015_public_v4.csv" 
# url="https://www.eia.gov/consumption/residential/data/2015/csv/recs2015_public_v4.csv"

## if the file doesn't exist
# if [ ! -f "$file" ]; then
#   wget $url
# fi

# b - extract header row and output to a file with one name per line
# new_file="recs_names.txt"

## delete new_file if it is already present
# if [ -f "$new_file" ]; then
#   rm "$new_file"
# fi

# <$file head -n1 | tr , \\n > $new_file

cols=$(
  <$file head -n1 | tr , \\n | grep -nE $pattern | cut -d: -f1 | tr \\n ,  
# <$new_file grep -nE $pattern | cut -d: -f1 | tr \\n ,    
)
cols=${cols%?} # delete the last comma
# echo $cols

# d - cut out the appropriate columns and save as recs_brrweights.csv
cut -d, -f$cols $file # > recs_brrweights.csv

# 79: -------------------------------------------------------------------------
