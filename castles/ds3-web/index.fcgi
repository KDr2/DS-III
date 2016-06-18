#!/bin/bash
HOME=${HOME-/home/kdr2}
source $HOME/.bashrc &>/dev/null
ds3 env perl &>/dev/null
export PATH_INFO=$SCRIPT_URL
#./main.pl fastcgi
plackup main.pl -r -s FCGI