#!/bin/bash

DEV=${1-wlan0}
WPA_CONF=/etc/wpa_supplicant/wpa-$DEV.conf

if [ $(ifconfig $DEV|grep "inet\s"|wc -l) -ge 1 ]; then
    echo "$DEV is connected."
else
    pidof wpa_supplicant && kill -9 $(pidof wpa_supplicant)
    wpa_supplicant -B -i $DEV -c $WPA_CONF
    dhclient $DEV
fi
