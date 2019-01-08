#!/bin/bash

# Text formatting
readonly BOLD=$(tput bold)
readonly RED=$(tput setaf 1)
readonly NORMAL=$(tput sgr0)

promptContinue() {
    read -n 1 -s -r -p "${BOLD}Press ctrl+c to cancel or any key to continue...${NORMAL}"
    printf "\n\n"
}

echo "NOTE: Python 2.7.x is required to use gditools, Python 3.x.x will not work."
echo "Your installed Python version is: ${BOLD}${RED}$(python --version 2>&1)${NORMAL}"
promptContinue

echo "NOTE: If you have the REDUMP game set, they add some non-standard track data to"
echo "their gdi files that may or may not work correctly. TOSEC or TruRip are preferred."
promptContinue

echo "Instructions:"
echo "Make sure to copy this script as well as the gdishrink.py, gditools.py, and iso9660.py"
echo "files into your directory of Dreamcast game folders or it won't work. The folder must"
echo "contain sub-folders that each have one game's .gdi file and associated image files."
echo " "
echo "For example:"
echo " "
echo "├── Crazy\ Taxi\ v1.004\ (1999)(Sega)(NTSC)(US)[!][10S\ 51035]"
echo "│   ├── Crazy\ Taxi\ v1.004\ (1999)(Sega)(NTSC)(US)[!][10S\ 51035].gdi"
echo "│   ├── track01.bin"
echo "│   ├── track02.raw"
echo "│   └── track03.bin"
echo "├── Dead\ or\ Alive\ 2\ v1.100\ (2000)(Tecmo)(NTSC)(US)[!]"
echo "│   ├── Dead\ or\ Alive\ 2\ v1.100\ (2000)(Tecmo)(NTSC)(US)[!].gdi"
echo "│   ├── track01.bin"
echo "│   ├── track02.raw"
echo "│   └── track03.bin"
echo "├── Rez\ v1.003\ (2001)(Sega)(PAL)(M6)[!]"
echo "|   ├── Rez\ v1.003\ (2001)(Sega)(PAL)(M6)[!].gdi"
echo "|   ├── track01.bin"
echo "|   ├── track02.raw"
echo "|   └── track03.bin"
echo "├── gdishrink.py"
echo "├── gditools.py"
echo "├── iso9660.py"
echo "└── shrink_all.sh"
echo " "
echo " "
echo "Are you sure you want to shrink all .gdi images ${BOLD}${RED}IN PLACE${NORMAL} in directory: $(pwd)"
echo "${BOLD}${RED}IMPORTANT:${NORMAL} Make sure you have backups of the original files, as this process is not reversible."
promptContinue

numthreads=2
read -p "How many threads would you like to use (recommended 2 for HDDs and 10 for SSDs): " numthreads
if [ -n "$numthreads" ] && [ "$numthreads" -eq "$numthreads" ] 2>/dev/null; then
    echo "${BOLD}Using $numthreads threads${NORMAL}"
    find . -depth -mindepth 2 -maxdepth 2 -name "*.gdi" -exec echo -ne '"{}"\n' \; | sort | xargs -P$numthreads -n1 bash -c 'echo "Shinking $0" && python ./gdishrink.py "$0" > /dev/null 2>&1 && printf "Finished $0\n"'
    echo "${BOLD}Done!${NORMAL}"
else
    echo "${BOLD}${RED}You must enter a valid number of threads${NORMAL}"
fi

