#!/bin/bash


# Set this to the name of your main latex-file (e.g. main.tex) without the ending
# The generated file should have the same basename
LATEX_MAIN_DOCUMENT="main"


# Additional software required:
# - inotifywait and notify-send (contained in  'libnotify' on most operating systems)


red='\e[0;31m'
green='\e[0;32m'
yellow='\e[0;33m'
NC='\e[0m' #no color

function wait_fixed(){
  echo "Press key to try again!"
  read
}

if [ "$1" != "" ] ; then
  LATEX_MAIN_DOCUMENT="$1"
fi

while [ 1 ] ; do

  make MAIN_DOCUMENT=$LATEX_MAIN_DOCUMENT
  if [ $? -ne 0 ] ; then
    wait_fixed
    continue
  fi

  clear
  echo "Last compilation done at `date`"
  N_WARNINGS=$(grep  Warning "$LATEX_MAIN_DOCUMENT".log)
  if [ "$N_WARNINGS" -gt 0 ] ; then
    echo -e "\n${yellow}Warnings ($N_WARNINGS):\n"
    sed -n -e "/Warning/,/^$/ p" "$LATEX_MAIN_DOCUMENT".log
    echo -e "${NC}\n\n\n\n"
  fi

  N_ERRORS=$(grep Error "$LATEX_MAIN_DOCUMENT".log)
  if [ "$N_ERRORS" -gt 0 ] ; then
    echo -e "\n${red}Errors ($N_ERRORS):\n"
    sed -n -e "/Error/,/^$/ p" "$LATEX_MAIN_DOCUMENT".log
    echo -e "${NC}\n\n\n\n"
  fi


  if [ "$N_WARNINGS" -eq 0 ] && [ "$N_ERRORS" -eq 0 ] ; then
    echo -e "${green}"
    echo '       /(|'
    echo '      (  :'
    echo '     __\  \  _____'
    echo '   (____)  `|'
    echo '  (____)|   |'
    echo '   (____).__|'
    echo '    (___)__.|_____'
    echo -e "${NC}"
  fi 

  

	notify-send -t 3000 "pdflatex" "compilation done"

	inotifywait -q -e modify *.tex */*.tex */*.bib
done


