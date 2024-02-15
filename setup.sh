#!/bin/bash     

R -e "if (!requireNamespace('tidyverse', quietly = TRUE)) install.packages('tidyverse')"

python3 -m venv env
./env/bin/pip3 install --upgrade pip
./env/bin/pip3 install -r requirements.txt