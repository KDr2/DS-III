# qemu startup script
export QEMU_X64=qemu-system-x86_64
export QEMU_X86=qemu-system-i386
export QEMU_ARM=qemu-system-arm

if [ X$GDB != X ]; then
    export GDB_OPT="-gdb tcp::4321"
fi

export HDDROOT=$HOME/Work/qemu
export HDD_DEBIAN64=$HDDROOT/debian64.vdi

if [ X$QEMU_HDD = X ]; then
    export QEMU_HDD=$HDD_DEBIAN64
fi

export DEFAULT_NET_OPT="-net nic,macaddr=52:54:00:12:34:56 -net tap,ifname=tap1,script=no"
#export DEFAULT_NET_OPT="-net nic,macaddr=52:54:00:12:34:57 -net tap,ifname=tap0,script=no"
#export DEFAULT_NET_OPT="-net nic,macaddr=52:54:00:12:34:56 -net user"
#export DEFAULT_NET_OPT="-net nic,macaddr=52:54:00:12:34:57 -net user"

export BUILD_DIR_QEMU_36=/opt/kernel/build/3.6-qemu/build
export X86_IMAGE=arch/x86/boot/bzImage

$QEMU_X64 -enable-kvm -smp 4 -m 1024 $GDB_OPT \
    -kernel $BUILD_DIR_QEMU_36/$X86_IMAGE  \
    -hda $QEMU_HDD -append "root=/dev/sda1" \
    $DEFAULT_NET_OPT
