#!/bin/bash

DEV=${1-eth0}

if [ $(ifconfig $DEV|grep "inet\s"|wc -l) -ge 1 ]; then
    echo "$DEV is connected."
else
    dhclient $DEV
fi
