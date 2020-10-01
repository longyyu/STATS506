#!/usr/bin/env bash

# Stats 506, Fall 2020
# Case Study 1: tidyverse 
# 
# This script downloads the 2009 RECS data for use in the tidyverse case
# study. In the case study, we answer the question:
#
# > Which Census Division has the highest proportion of single-family
#   attached homes? 
#
# Author(s): James Henderson
# Updated: September 21, 2020
# 79: -------------------------------------------------------------------------

# files needed: ---------------------------------------------------------------
url_base="https://www.eia.gov/consumption/residential/data/2009/"
data_file=recs2009_public.csv
codebook=recs2009_public_codebook.xlsx
weight_file=recs2009_public_repweights.csv

# downloads: ------------------------------------------------------------------

## data file
if [ ! -f $data_file ]; then
    wget $url_base/csv/$data_file
fi

## codebook
if [ ! -f $codebook ]; then
    wget $url_base/xls/$codebook
fi

## replicate weights
if [ ! -f $weight_file ]; then
    wget $url_base/csv/$weight_file
fi

# 79: -------------------------------------------------------------------------
