#!/bin/bash

export DISPLAY=":3"
Xvnc -SecurityTypes none -geometry 1840x1000 -rfbport 3307 $DISPLAY &
sleep 5
icewm
