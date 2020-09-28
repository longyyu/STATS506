#!/bin/env bash

# Column Selector
# Takes two arguments, `file` and `pattern`, 
#   and return the extracted columns
# Week 1 Activity - Stats 506, Fall 2020
# Author: Yanyu Long, longyyu@umich.edu
# Updated: Sep 29, 2020

# 79: -------------------------------------------------------------------------
file=$1
pattern=$2

# extracts the column number of variables specified by `pattern`
#   and paste them with "," into a single string `cols`
cols=$(
  <$file head -n1 | tr , \\n | grep -nE $pattern | cut -d: -f1 | paste -d, -s 
)

# d - cut out the appropriate columns and save as recs_brrweights.csv
cut -d, -f$cols $file

# 79: -------------------------------------------------------------------------
