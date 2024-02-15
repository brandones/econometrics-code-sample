#!/bin/bash

set -e  # die on error

# ./0_downloading/data.sh down
# ./env/bin/python3 0_downloading/download-wits-imports.py

R -f 1_cleaning/harvard-complexity-clean.R
R -f 1_cleaning/wits-clean.R
R -f 2_merging/merge-wits-with-complexity.R
R -f 3_analysis/analysis-real-resource-dependency.R
