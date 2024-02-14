# Check if the first argument is 'up' or 'down'

if [ "$1" = "up" ]; then
  zip data.zip *.dta
  gdrive files upload --parent 14zb1fSfkNpCqnU38w34pjCRAkWXT6EM1 child-care-limm-data.zip
elif [ "$1" = "down" ]; then
  ID=gdrive files list | grep Data | cut -f1 -d' '
  gdrive files download $ID
  unzip child-care-limm-data.zip
else
    echo "error: first argument must be 'up' or 'down'"
fi
