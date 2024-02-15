#!/bin/bash

if [ "$1" = "up" ]; then
  zip data.zip data_0_raw/*
  gdrive files upload --parent 14zb1fSfkNpCqnU38w34pjCRAkWXT6EM1 code-sample.zip
elif [ "$1" = "down" ]; then
  ID=gdrive files list | grep Data | cut -f1 -d' '
  gdrive files download $ID
  unzip code-sample.zip
else
    echo "error: first argument must be 'up' or 'down'"
fi
