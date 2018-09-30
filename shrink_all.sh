#!/bin/bash
directory=$(pwd) 
echo "Are you sure you want to shrink all .gdi images IN PLACE in directory: $directory"
read -n 1 -s -r -p "Press ctrl+c to cancel or any key to continue..."
printf "\n\n"

find . -depth -mindepth 2 -maxdepth 2 -name "*.gdi" -exec echo -ne '"{}"\n' \; | sort | xargs -P10 -n1 bash -c 'echo "Shinking $0" && python2.7 ./gdishrink.py "$0" > /dev/null 2>&1 && printf "Finished $0\n"'
