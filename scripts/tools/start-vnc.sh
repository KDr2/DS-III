#!/bin/bash

PORT=3308
DISPLAY=":4"


# First, use `vncpasswd ~/.vnc/passwd` to set a password.
if [[ -f ~/.vnc/passwd ]]; then
    AUTH_TYPE=(-PasswordFile ~/.vnc/passwd)
else
    AUTH_TYPE=(-SecurityTypes none)
fi

# We may use `-xstartup icewm` here, but that causes some problems
# like fonts and application list loading. I guess the vncserver
# resets some environment varibles in its running process.
tigervncserver ${AUTH_TYPE[@]} \
               -localhost no -rfbport $PORT \
               -noxstartup -autokill no \
               -geometry 1840x1000 \
               $DISPLAY &

# wait for the XServer to be started
sleep 4

# start icewm
icewm -d $DISPLAY
