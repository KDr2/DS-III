#!/bin/bash

export TAP_DEV=tap0
export VDE_USER=kdr2
export VDE_GROUP=kdr2
export VDE_COMMDIR=/tmp/switch.kdr2

ifconfig $TAP_DEV 2&>1 > /dev/null
if [ $? != 0 ]; then
    vde_tunctl -u $VDE_USER -t $TAP_DEV
fi

vde_switch --daemon \
           -fstp -sock $VDE_COMMDIR \
           --mode 770 \
           --group $VDE_GROUP \
           --tap $TAP_DEV

ifconfig tap0 192.168.7.1
#route add -host 192.168.7.22 dev tap0
