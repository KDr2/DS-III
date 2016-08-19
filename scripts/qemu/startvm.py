#!/usr/bin/env python
#-*- coding: utf-8 -*-

import os
import sys
import yaml

QEMU_EXE={"x86": "/usr/bin/qemu-system-i386",
          "x64": "/usr/bin/qemu-system-x86_64",
          "x86_64": "/usr/bin/qemu-system-x86_64",
          "arm": "/usr/bin/qemu-system-arm",
          }

def guess_vmtop(vname):
    if(vname.find('/')>=0): return vname
    vmdirs = os.getenv("QEMU_VM_PATH")
    if not vmdirs: return vname
    for d in vmdirs.split(":"):
        cf = os.path.join(d, vname, "vm-config.yaml")
        try:
            os.stat(cf)
            return os.path.join(d, vname)
        except:
            pass
    #endfor
    return vname

def get_config():
    vname = sys.argv[1] if len(sys.argv)>1 else os.getcwd()
    vmdir = guess_vmtop(vname)
    vmconfig = os.path.join(vmdir, "vm-config.yaml")
    fvmconf = open(vmconfig)
    conf = yaml.load(fvmconf)
    fvmconf.close()
    conf['top'] = vmdir
    return conf

def parse_config(config):
    ret = []
    vmdir = config['top']
    # baic.arch
    arch = config['basic'].get('arch', 'x64').lower()
    ret.append(QEMU_EXE[arch])
    # basic.kvm
    kvm = config['basic'].get('kvm', False)
    if kvm: ret.append("-enable-kvm")
    # baic.mem
    mem = config['basic'].get('mem', 512)
    ret.append("-m")
    ret.append(str(mem))
    # basic.smp
    smp = config['basic'].get('smp', 0)
    if smp:
        ret.append("-smp")
        ret.append(str(smp))
    #endif

    #hdd
    hdd = config.get("hdd", {})
    for k,v in hdd.iteritems():
        ret.append("-%s" % k)
        ret.append(os.path.join(vmdir, v))
    #endfor

    #net
    net = config.get("net", [])
    for n in net:
        ret.append("-net")
        ret.append(n)
    #endfor

    #kernel
    k = config.get("kernel", {})
    if k.get("outside", False):
        ret.append("-kernel")
        ret.append(k.get("image"))
        ret.append("-append")
        ret.append(" ".join(k.get("append", [])))

    #gdb
    gdb = config.get("gdb", {})
    if gdb.get("enable", False):
        ret.append("-gdb")
        ret.append("tcp:%s" % gdb.get("tcp"))

    #misc
    misc = config.get("misc", {})
    if(misc.get("curses")): ret.append("-curses")
    if(misc.get("nographic")): ret.append("-nographic")
    if(misc.get("usb")): ret.append("-usb")

    return ret


def main():
    args = parse_config(get_config())
    os.execv(args[0], args)


if __name__ == '__main__':
    main()
