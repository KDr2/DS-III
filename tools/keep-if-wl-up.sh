#!/bin/bash

DEV=${1-wlan0}
WPA_CONF=/etc/wpa_supplicant/wpa-$DEV.conf

# content of the conf file:
# network={
#   ssid="SSID"
#   scan_ssid=1
#   proto=WPA RSN
#   key_mgmt=WPA-PSK
#   pairwise=CCMP TKIP
#   group=CCMP TKIP
#   psk="PASSWORD"
# }

if [ $(ifconfig $DEV|grep "inet\s"|wc -l) -ge 1 ]; then
    echo "$DEV is connected."
else
    pidof wpa_supplicant && kill -9 $(pidof wpa_supplicant)
    wpa_supplicant -B -i $DEV -c $WPA_CONF
    dhclient $DEV
fi
