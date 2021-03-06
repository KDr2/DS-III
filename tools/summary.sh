#!/usr/bin/env bash
# This program is part of Aspersa (http://code.google.com/p/aspersa/)

# ########################################################################
# A script to summarize system information in a nice way.
# Goals: work well on Linux; create a compact diff-able report that is
# easy to paste into a wiki or email, and easy to scan and compare too.
#
# Usage: $ wget -O- http://aspersa.googlecode.com/svn/trunk/summary |bash
# Options are set through the ASPERSA_SKIP environment variable.  Set this
# variable to a comma-separated list of things you want to omit.
# Options:
#  MOUNT:   Don't print out mounted filesystems and disk fullness.
#  NETWORK: Don't print out information on network controllers & config.
#  PROCESS: Don't print out top processes and vmstat information.
#
# Authors:
#  Baron Schwartz
#  Kevin van Zonneveld (kvz@php.net || http://kevin.vanzonneveld.net)
# ########################################################################

# ########################################################################
# Globals, helper functions
# ########################################################################

# The temp files are for storing working results so we don't call commands many
# times (gives inconsistent results, maybe adds load on things I don't want to
# such as RAID controllers).  They must not exist -- if they did, someone would
# symlink them to /etc/passwd and then run this program as root.  Call this
# function with "rm" or "touch" as an argument.
temp_files() {
   for file in /tmp/aspersa /tmp/aspersa2; do
      case "$1" in
      touch)
         if ! touch "${file}"; then
            echo "I can't make my temp file ${file}";
            exit 1;
         fi
         ;;
      rm)
         rm -f "${file}"
         ;;
      esac
   done
}

# Print a space-padded string into $line.  Then translate spaces to hashes, and
# underscores to spaces.  End result is a line of hashes with words at the
# start.
section () {
   line="$(printf '#_%-60s' "$1_")"
   line="${line// /#}"
   printf "%s\n" "${line//_/ }"
}

# Print a "name | value" line.
name_val() {
   printf "%12s | %s\n" "$1" "$2"
}

# Converts a value to units of power of 2. Stores the result into a variable
# with the name you specify.  Usage: shorten $value 'variable_name'
shorten() {
   unit=k
   size=1024
   if [ $1 -ge 1099511627776 ] ; then
      size=1099511627776
      unit=T
   elif [ $1 -ge 1073741824 ] ; then
      size=1073741824
      unit=G
   elif [ $1 -ge 1048576 ] ; then
      size=1048576
      unit=M
   fi
   result=$(echo "$1 $size" | awk '{printf "%.2f", $1 / $2}')
   export "$2"="$result$unit"
}

# ##############################################################################
# Function to take a file and collapse it into an aggregated list.  This
# function works on $1, which it expects to be created with 'sort |
# uniq -c'.  Leading whitespace is deleted.  The result will look like
# "4xabc, 1xdef"
# ##############################################################################
group_concat () {
   sed -e '{H; $!d}' -e 'x' -e 's/\n[[:space:]]*\([[:digit:]]*\)[[:space:]]*/, \1x/g' -e 's/[[:space:]][[:space:]]*/ /g' -e 's/, //' ${1}
   # In words: save the whole file into the hold space,
   # {H; $!d}
   # Swap it back into the pattern space,
   # x
   # Join lines with a comma, delete leading whitespace, and put an 'x' between
   # the number and the text that follows,
   # s/\n[[:space:]]*\([[:digit:]]*\)[[:space:]]*/, \1x/g
   # Collapse whitespace,
   # s/[[:space:]][[:space:]]*/ /g
   # And delete the leading comma-space.
   # s/, //
}

# ##############################################################################
# Functions for parsing specific files and getting desired info from them.
# These are called from within main() and are separated so they can be tested
# easily.  The calling convention is that the data they need to run is prepared
# first by putting it into /tmp/aspersa.  Then code that's testing just needs to
# put sample data into /tmp/aspersa and call it.
# ##############################################################################

# ##############################################################################
# Parse Linux's /proc/cpuinfo, which should be stored in /tmp/aspersa.
# ##############################################################################
parse_proc_cpuinfo () {
   # Physical processors are indicated by distinct 'physical id'.  Virtual CPUs
   # are indicated by paragraphs -- one per paragraph.  We assume that all
   # processors are identical, i.e. that there are not some processors with dual
   # cores and some with quad cores.
   virtual=$(grep -c ^processor /tmp/aspersa);
   physical=$(grep 'physical id' /tmp/aspersa | sort -u | wc -l);
   cores=$(grep 'cpu cores' /tmp/aspersa | head -n 1 | cut -d: -f2);

   # Older kernel won't have 'physical id' or 'cpu cores'.
   if [ "${physical}" = "0" ]; then physical=${virtual}; fi
   if [ -z "${cores}" ]; then cores=0; fi

   # Test for HTT; cannot trust the 'ht' flag.  If physical * cores < virtual,
   # then hyperthreading is in use.
   cores=$((${cores} * ${physical}));
   if [ ${cores} -gt 0 -a $cores -lt $virtual ]; then htt=yes; else htt=no; fi

   name_val "Processors" "physical = ${physical}, cores = ${cores}, virtual = ${virtual}, hyperthreading = ${htt}"

   awk -F: '/cpu MHz/{print $2}' /tmp/aspersa \
      | sort | uniq -c > /tmp/aspersa2
   name_val "Speeds" "$(group_concat /tmp/aspersa2)"

   awk -F: '/model name/{print $2}' /tmp/aspersa \
      | sort | uniq -c > /tmp/aspersa2
   name_val "Models" "$(group_concat /tmp/aspersa2)"

   awk -F: '/cache size/{print $2}' /tmp/aspersa \
      | sort | uniq -c > /tmp/aspersa2
   name_val "Caches" "$(group_concat /tmp/aspersa2)"
}

# ##############################################################################
# Parse the output of 'free -b', which should be stored in /tmp/aspersa.
# ##############################################################################
parse_free_minus_b () {
   shorten "$(awk '/Mem/{print $2}' /tmp/aspersa)" total
   name_val Total ${total}

   physical=$(awk '/Mem/{print $3}' /tmp/aspersa);
   swap=$(awk '/Swap/{print $3}' /tmp/aspersa);
   virtual=$(($physical + $swap))

   free=$(awk '/Mem/{print $4}' /tmp/aspersa);
   shorten ${free} free
   name_val Free ${free}

   shorten ${physical} physical
   shorten ${swap} swap
   shorten ${virtual} virtual
   name_val Used "physical = ${physical}, swap = ${swap}, virtual = ${virtual}"

   buffers=$(awk '/Mem/{print $6}' /tmp/aspersa);
   shorten ${buffers} buffers
   name_val Buffers ${buffers}

   cached=$(awk '/Mem/{print $7}' /tmp/aspersa);
   shorten ${cached} cached
   name_val Caches ${cached}
}

# ##############################################################################
# Parse memory devices from the output of 'dmidecode', which should be stored in
# /tmp/aspersa.
# ##############################################################################
parse_dmidecode_mem_devices () {
   echo "  Locator   Size     Speed             Form Factor   Type          Type Detail"
   echo "  ========= ======== ================= ============= ============= ==========="
   # Print paragraphs containing 'Memory Device\n', extract the desired bits,
   # concatenate them into one long line, then format as a table.  The data
   # comes out in this order for each paragraph:
   # $2  Size         2048 MB
   # $3  Form Factor  <OUT OF SPEC>
   # $4  Locator      DIMM1
   # $5  Type         <OUT OF SPEC>
   # $6  Type Detail  Synchronous
   # $7  Speed        667 MHz (1.5 ns)
   sed    -e '/./{H;$!d;}' \
          -e 'x;/Memory Device\n/!d;' \
          -e 's/: /:/g' \
          -e 's/</{/g' \
          -e 's/>/}/g' \
          -e 's/[ \t]*\n/\n/g' \
       /tmp/aspersa \
       | awk -F: '/Size|Type|Form.Factor|Type.Detail|[^ ]Locator/{printf("|%s", $2)}/Speed/{print "|" $2}' \
       | sed -e 's/No Module Installed/{EMPTY}/' \
       | sort \
       | awk -F'|' '{printf("  %-9s %-8s %-17s %-13s %-13s %-8s\n", $4, $2, $7, $3, $5, $6);}'
}

# ##############################################################################
# Parse the output of 'netstat -antp' which should be in /tmp/aspersa.
# ##############################################################################
parse_ip_s_link () {
   echo "  interface  rx_bytes rx_packets  rx_errors   tx_bytes tx_packets  tx_errors"
   echo "  ========= ========= ========== ========== ========== ========== =========="

   awk '/^[1-9][0-9]*:/ {
      save["iface"] = substr($2, 0, index($2, ":") - 1);
      new = 1;
   }
   $0 !~ /[^0-9 ]/ {
      if ( new == 1 ) {
         new = 0;
         save["bytes"] = $1;
         save["packs"] = $2;
         save["errs"]  = $3;
      }
      else {
         printf "  %-8s %10d %10d %10d %10d %10d %10d\n", save["iface"], save["bytes"], save["packs"], save["errs"], $1, $2, $3;
      }
   }' /tmp/aspersa
}

# ##############################################################################
# Parse the output of 'netstat -antp' which should be in /tmp/aspersa.
# ##############################################################################
parse_netstat () {
   echo "  Connections from remote IP addresses"
   awk '$1 ~ /^tcp/ && $5 ~ /^[1-9]/ {
      print substr($5, 0, index($5, ":") - 1);
   }' /tmp/aspersa | sort | uniq -c \
      | awk '{printf "    %-15s %5d\n", $2, $1}' \
      | sort -n -t . -k 1,1 -k 2,2 -k 3,3 -k 4,4
   echo "  Connections to local IP addresses"
   awk '$1 ~ /^tcp/ && $5 ~ /^[1-9]/ {
      print substr($4, 0, index($4, ":") - 1);
   }' /tmp/aspersa | sort | uniq -c \
      | awk '{printf "    %-15s %5d\n", $2, $1}' \
      | sort -n -t . -k 1,1 -k 2,2 -k 3,3 -k 4,4
   echo "  Connections to top 10 local ports"
   awk '$1 ~ /^tcp/ && $5 ~ /^[1-9]/ {
      print substr($4, index($4, ":") + 1);
   }' /tmp/aspersa | sort | uniq -c | sort -rn | head -n10 \
      | awk '{printf "    %-15s %5d\n", $2, $1}' | sort
   echo "  States of connections"
   awk '$1 ~ /^tcp/ {
      print $6;
   }' /tmp/aspersa | sort | uniq -c | sort -rn \
      | awk '{printf "    %-15s %5d\n", $2, $1}' | sort
}

# ##############################################################################
# Parse the joined output of 'mount' and 'df -hP', which should be stored in
# /tmp/aspersa.
# ##############################################################################
parse_filesystems () {
   # Filesystem names and mountpoints can be very long.  We try to align things
   # as nicely as possible by making columns only as wide as needed.  This
   # requires two passes through the file.  The first pass finds the max size of
   # these columns and prints out a printf spec, and the second prints out the
   # file nicely aligned.
   spec="$(awk '
      BEGIN {
         f=10;
         m=0;
         t=0;
      }
      /./ {
         if ( length($1) > f ) {
            f=length($1);
         }
         if ( length($11) > m ) {
            m=length($11);
         }
         if ( length($10) > t ) {
            t=length($10);
         }
      }
      END{
         print "%-" f "s %5s %4s %-" t "s %-" m "s %s";
      }
   ' /tmp/aspersa)";
   awk "
      BEGIN {
         spec=\"  ${spec}\n\";
         printf spec, \"Filesystem\", \"Size\", \"Used\", \"Type\", \"Opts\", \"Mountpoint\";
      }
      {
         printf spec, \$1, \$2, \$5, \$10, \$11, \$6
      }" /tmp/aspersa
}

# ##############################################################################
# Parse the output of fdisk -l, which should be in /tmp/aspersa; there might be
# multiple fdisk -l outputs in the file.
# ##############################################################################
parse_fdisk () {
   awk '
      BEGIN {
         format="%-12s %4s %10s %10s %18s\n";
         printf(format, "Device", "Type", "Start", "End", "Size");
         printf(format, "============", "====", "==========", "==========", "==================");
      }
      /Disk.*bytes/ {
         disk = substr($2, 1, length($2) - 1);
         size = $5;
         printf(format, disk, "Disk", "", "", size);
      }
      /Units/ {
         units = $9;
      }
      /^\/dev/ {
         if ( $2 == "*" ) {
            start = $3;
            end   = $4;
         }
         else {
            start = $2;
            end   = $3;
         }
         printf(format, $1, "Part", start, end, (end - start) * units);
      }
   ' /tmp/aspersa
}

# ##############################################################################
# Parse the output of dmesg, which should be in /tmp/aspersa, and detect
# virtualization.
# ##############################################################################
parse_virtualization_dmesg () {
   if grep -qi vmware /tmp/aspersa; then
      echo "VMWare";
   elif grep -qi vmxnet /tmp/aspersa; then
      echo "VMWare";
   elif grep -qi 'paravirtualized kernel on vmi' /tmp/aspersa; then
      echo "VMWare";
   elif grep -qi 'paravirtualized kernel on xen' /tmp/aspersa; then
      echo "Xen";
   elif grep -qi qemu /tmp/aspersa; then
      echo "QEmu";
   elif grep -qi 'paravirtualized kernel on KVM' /tmp/aspersa; then
      echo "KVM";
   elif grep -qi 'hd.: Virtual .., ATA.*drive' /tmp/aspersa; then
      echo "Microsoft VirtualPC";
   elif grep -qi 'Xen virtual console' /tmp/aspersa; then
      echo "Xen";
   fi
}

# ##############################################################################
# Parse the output of lspci, which should be in /tmp/aspersa, and detect
# Ethernet cards.
# ##############################################################################
parse_ethernet_controller_lspci () {
   grep -i ethernet /tmp/aspersa | cut -d: -f3 | while read line; do
      name_val Controller "${line}"
   done
}

# ##############################################################################
# Parse the output of lspci, which should be in /tmp/aspersa, and detect RAID
# controllers.
# ##############################################################################
parse_raid_controller_lspci () {
   if grep -q "RAID bus controller: LSI Logic / Symbios Logic MegaRAID SAS" /tmp/aspersa; then
      echo 'LSI Logic MegaRAID SAS'
   elif grep -q "Fusion-MPT SAS" /tmp/aspersa; then
      echo 'Fusion-MPT SAS'
   elif grep -q "RAID bus controller: LSI Logic / Symbios Logic Unknown" /tmp/aspersa; then
      echo 'LSI Logic Unknown'
   elif grep -q "RAID bus controller: Adaptec AAC-RAID" /tmp/aspersa; then
      echo 'AACRAID'
   elif grep -q "3ware [0-9]* Storage Controller" /tmp/aspersa; then
      echo '3Ware'
   elif grep -q "Hewlett-Packard Company Smart Array" /tmp/aspersa; then
      echo 'HP Smart Array'
   elif grep -q " RAID bus controller: " /tmp/aspersa; then
      awk -F: '/RAID bus controller\:/ {print $3" "$5" "$6}' /tmp/aspersa
   fi
}

# ##############################################################################
# Parse the output of dmesg, which should be in /tmp/aspersa, and detect RAID
# controllers.
# ##############################################################################
parse_raid_controller_dmesg () {
   pat='scsi[0-9].*: .*'
   if grep -qi "${pat}megaraid" /tmp/aspersa; then
      echo 'LSI Logic MegaRAID SAS'
   elif grep -q "Fusion MPT SAS" /tmp/aspersa; then
      echo 'Fusion-MPT SAS'
   elif grep -q "${pat}aacraid" /tmp/aspersa; then
      echo 'AACRAID'
   elif grep -q "${pat}3ware [0-9]* Storage Controller" /tmp/aspersa; then
      echo '3Ware'
   fi
}

# ##############################################################################
# Parse the output of "hpacucli ctrl all show config", which should be stored in
# /tmp/aspersa
# ##############################################################################
parse_hpacucli () {
   grep 'logicaldrive\|physicaldrive' /tmp/aspersa
}

# ##############################################################################
# Parse the output of arcconf, which should be stored in /tmp/aspersa
# ##############################################################################
parse_arcconf () {
   model=$(awk -F: '/Controller Model/{print $2}' /tmp/aspersa)
   chan="$(awk -F: '/Channel description/{print $2}' /tmp/aspersa)"
   cache="$(awk -F: '/Installed memory/{print $2}' /tmp/aspersa)"
   status="$(awk -F: '/Controller Status/{print $2}' /tmp/aspersa)"
   name_val Specs "${model/ /},${chan},${cache} cache,${status}"

   battery=$(grep -A5 'Controller Battery Info' /tmp/aspersa \
      | awk '/Capacity remaining/ {c=$4}
             /Status/             {s=$3}
             /Time remaining/     {t=sprintf("%dd%dh%dm", $7, $9, $11)}
             END                  {printf("%d%%, %s remaining, %s", c, t, s)}')
   name_val Battery "${battery}"

   # ###########################################################################
   # Logical devices
   # ###########################################################################
   echo
   echo "  LogicalDev Size      RAID Disks Stripe Status  Cache"
   echo "  ========== ========= ==== ===== ====== ======= ======="
   for dev in $(awk '/Logical device number/{print $4}' /tmp/aspersa); do
      sed -n -e "/^Logical device .* ${dev}$/,/^$\|^Logical device number/p" \
         /tmp/aspersa \
      | awk '
         /Logical device name/               {d=$5}
         /Size/                              {z=$3 " " $4}
         /RAID level/                        {r=$4}
         /Group [0-9]/                       {g++}
         /Stripe-unit size/                  {p=$4 " " $5}
         /Status of logical/                 {s=$6}
         /Write-cache mode.*Ena.*write-back/ {c="On (WB)"}
         /Write-cache mode.*Ena.*write-thro/ {c="On (WT)"}
         /Write-cache mode.*Disabled/        {c="Off"}
         END {
            printf("  %-10s %-9s %4d %5d %-6s %-7s %-7s\n",
               d, z, r, g, p, s, c);
         }'
   done

   # ###########################################################################
   # Physical devices
   # ###########################################################################
   echo
   echo "  PhysiclDev State   Speed         Vendor  Model        Size        Cache"
   echo "  ========== ======= ============= ======= ============ =========== ======="

   # Find the paragraph with physical devices, tabularize with assoc arrays.
   tempresult=""
   sed -n -e '/Physical Device information/,/^$/p' /tmp/aspersa \
      | awk -F: '
         /Device #[0-9]/ {
            device=substr($0, index($0, "#"));
            devicenames[device]=device;
         }
         /Device is a/ {
            devices[device ",isa"] = substr($0, index($0, "is a") + 5);
         }
         /State/ {
            devices[device ",state"] = substr($2, 2);
         }
         /Transfer Speed/ {
            devices[device ",speed"] = substr($2, 2);
         }
         /Vendor/ {
            devices[device ",vendor"] = substr($2, 2);
         }
         /Model/ {
            devices[device ",model"] = substr($2, 2);
         }
         /Size/ {
            devices[device ",size"] = substr($2, 2);
         }
         /Write Cache/ {
            if ( $2 ~ /Enabled .write-back./ )
               devices[device ",cache"] = "On (WB)";
            else
               if ( $2 ~ /Enabled .write-th/ )
                  devices[device ",cache"] = "On (WT)";
               else
                  devices[device ",cache"] = "Off";
         }
         END {
            for ( device in devicenames ) {
               if ( devices[device ",isa"] ~ /Hard drive/ ) {
                  printf("  %-10s %-7s %-13s %-7s %-12s %-11s %-7s\n",
                     devices[device ",isa"],
                     devices[device ",state"],
                     devices[device ",speed"],
                     devices[device ",vendor"],
                     devices[device ",model"],
                     devices[device ",size"],
                     devices[device ",cache"]);
               }
            }
         }'

}

# ##############################################################################
# Parse the output of "lsiutil -i -s" from /tmp/aspersa
# ##############################################################################
parse_fusionmpt_lsiutil () {
   echo
   awk '/LSI.*Firmware/ { print " ", $0 }' /tmp/aspersa
   grep . /tmp/aspersa | sed -n -e '/B___T___L/,$ {s/^/  /; p}'
}

# ##############################################################################
# Parse the output of MegaCli64 -AdpAllInfo -aALL from /tmp/aspersa.
# ##############################################################################
parse_lsi_megaraid_adapter_info () {
   name=$(awk -F: '/Product Name/{print substr($2, 2)}' /tmp/aspersa);
   int=$(awk '/Host Interface/{print $4}' /tmp/aspersa);
   prt=$(awk '/Number of Backend Port/{print $5}' /tmp/aspersa);
   bbu=$(awk '/^BBU             :/{print $3}' /tmp/aspersa);
   mem=$(awk '/Memory Size/{print $4}' /tmp/aspersa);
   vdr=$(awk '/Virtual Drives/{print $4}' /tmp/aspersa);
   dvd=$(awk '/Degraded/{print $3}' /tmp/aspersa);
   phy=$(awk '/^  Disks/{print $3}' /tmp/aspersa);
   crd=$(awk '/Critical Disks/{print $4}' /tmp/aspersa);
   fad=$(awk '/Failed Disks/{print $4}' /tmp/aspersa);
   name_val Model "${name}, ${int} interface, ${prt} ports"
   name_val Cache "${mem} Memory, BBU ${bbu}"
}

# ##############################################################################
# Parse the output (saved in /tmp/aspersa) of
# /opt/MegaRAID/MegaCli/MegaCli64 -AdpBbuCmd -GetBbuStatus -aALL
# ##############################################################################
parse_lsi_megaraid_bbu_status () {
   charge=$(awk '/Relative State/{print $5}' /tmp/aspersa);
   temp=$(awk '/^Temperature/{print $2}' /tmp/aspersa);
   soh=$(awk '/isSOHGood:/{print $2}' /tmp/aspersa);
   name_val BBU "${charge}% Charged, Temperature ${temp}C, isSOHGood=${soh}"
}

# ##############################################################################
# Parse physical devices from the output (saved in /tmp/aspersa) of
# /opt/MegaRAID/MegaCli/MegaCli64 -LdPdInfo -aALL
# OR, it will also work with the output of
# /opt/MegaRAID/MegaCli/MegaCli64 -PDList -aALL
# ##############################################################################
parse_lsi_megaraid_devices () {
   echo
   echo "  PhysiclDev Type State   Errors Vendor  Model        Size"
   echo "  ========== ==== ======= ====== ======= ============ ==========="
   for dev in $(awk '/Device Id/{print $3}' /tmp/aspersa); do
      sed -e '/./{H;$!d;}' -e "x;/Device Id: ${dev}/!d;" /tmp/aspersa \
      | awk '
         /Media Type/                        {d=substr($0, index($0, ":") + 2)}
         /PD Type/                           {t=$3}
         /Firmware state/                    {s=$3}
         /Media Error Count/                 {me=$4}
         /Other Error Count/                 {oe=$4}
         /Predictive Failure Count/          {pe=$4}
         /Inquiry Data/                      {v=$3; m=$4;}
         /Raw Size/                          {z=$3}
         END {
            printf("  %-10s %-4s %-7s %6s %-7s %-12s %-7s\n",
               substr(d, 0, 10), t, s, me "/" oe "/" pe, v, m, z);
         }'
   done
}

# ##############################################################################
# Parse virtual devices from the output (saved in /tmp/aspersa) of
# /opt/MegaRAID/MegaCli/MegaCli64 -LdPdInfo -aALL
# OR, it will also work with the output of
# /opt/MegaRAID/MegaCli/MegaCli64 -LDInfo -Lall -aAll
# ##############################################################################
parse_lsi_megaraid_virtual_devices () {
   # Somewhere on the Internet, I found the following guide to understanding the
   # RAID level, but I don't know the source anymore.
   #    Primary-0, Secondary-0, RAID Level Qualifier-0 = 0
   #    Primary-1, Secondary-0, RAID Level Qualifier-0 = 1
   #    Primary-5, Secondary-0, RAID Level Qualifier-3 = 5
   #    Primary-1, Secondary-3, RAID Level Qualifier-0 = 10
   # I am not sure if this is always correct or not (it seems correct).  The
   # terminology MegaRAID uses is not clear to me, and isn't documented that I
   # am aware of.  Anyone who can clarify the above, please contact me.
   echo
   echo "  VirtualDev Size      RAID Level Disks SpnDpth Stripe Status  Cache"
   echo "  ========== ========= ========== ===== ======= ====== ======= ========="
   awk '
      /^Virtual Disk:/ {
         device              = $3;
         devicenames[device] = device;
      }
      /Number Of Drives/ {
         devices[device ",numdisks"] = substr($0, index($0, ":") + 1);
      }
      /^Name:/ {
         devices[device ",name"] = $2 > "" ? $2 : "(no name)";
      }
      /RAID Level/ {
         devices[device ",primary"]   = substr($3, index($3, "-") + 1, 1);
         devices[device ",secondary"] = substr($4, index($4, "-") + 1, 1);
         devices[device ",qualifier"] = substr($NF, index($NF, "-") + 1, 1);
      }
      /Span Depth/ {
         devices[device ",spandepth"] = substr($2, index($2, ":") + 1);
      }
      /Number of Spans/ {
         devices[device ",numspans"] = $4;
      }
      /^Size:/ {
         devices[device ",size"] = substr($0, index($0, ":") + 1);
      }
      /^State:/ {
         devices[device ",state"] = $2;
      }
      /^Stripe Size:/ {
         devices[device ",stripe"] = $3;
      }
      /^Current Cache Policy/ {
         devices[device ",wpolicy"] = $4 ~ /WriteBack/ ? "WB" : "WT";
         devices[device ",rpolicy"] = $5 ~ /ReadAheadNone/ ? "no RA" : "RA";
      }
      END {
         for ( device in devicenames ) {
            raid = 0;
            if ( devices[device ",primary"] == 1 ) {
               raid = 1;
               if ( devices[device ",secondary"] == 3 ) {
                  raid = 10;
               }
            }
            else {
               if ( devices[device ",primary"] == 5 ) {
                  raid = 5;
               }
            }
            printf("  %-10s %-9s %-10s %5d %7s %6s %-7s %s\n",
               device devices[device ",name"],
               devices[device ",size"],
               raid " (" devices[device ",primary"] "-" devices[device ",secondary"] "-" devices[device ",qualifier"] ")",
               devices[device ",numdisks"],
               devices[device ",spandepth"] "-" devices[device ",numspans"],
               devices[device ",stripe"], devices[device ",state"],
               devices[device ",wpolicy"] ", " devices[device ",rpolicy"]);
         }
      }' /tmp/aspersa
}

# ##############################################################################
# Simplifies vmstat and aligns it nicely.  We don't need the memory stats, the
# system activity is enough.
# ##############################################################################
format_vmstat () {
   awk '
      /procs/ {
         print  "  procs  ---swap-- -----io---- ---system---- --------cpu--------";
      }
      $0 !~ /procs/ {
         printf "  %2s %2s  %4s %4s %5s %5s %6s %6s %3s %3s %3s %3s %3s\n",
            $1, $2, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17;
      }
   ' /tmp/aspersa
}

# ##############################################################################
# The main() function is called at the end of the script.  This makes it
# testable.  Major bits of parsing are separated into functions for testability.
# As a general rule, we cannot 'cp' files from /proc, because they might be
# empty afterwards.  (I've seen 'cp /proc/cpuinfo' create an empty file.)  But
# 'cat' works okay.
# ##############################################################################
main () {

   # Begin by setting the $PATH to include some common locations that are not
   # always in the $PATH, including the "sbin" locations, and some common
   # locations for proprietary management software, such as RAID controllers.
   export PATH="${PATH}:/usr/local/bin:/usr/bin:/bin:/usr/libexec"
   export PATH="${PATH}:/usr/local/sbin:/usr/sbin:/sbin"
   export PATH="${PATH}:/usr/StorMan/:/opt/MegaRAID/MegaCli/";

   # Set up temporary files.
   temp_files "rm"
   temp_files "touch"
   section Aspersa_System_Summary_Report

   # ########################################################################
   # General date, time, load, etc
   # ########################################################################
   name_val "Date" "`date -u +'%F %T UTC'` (local TZ: `date +'%Z %z'`)"
   name_val "Hostname" `uname -n`
   name_val "Uptime" "`uptime | cut -dp -f2 | cut -c2-`"
   vendor="$(dmidecode -s system-manufacturer 2>/dev/null | sed 's/ *$//g')"
   if [ "${vendor}" ]; then
      product="$(dmidecode -s system-product-name 2>/dev/null | sed 's/ *$//g')"
      version="$(dmidecode -s system-version 2>/dev/null | sed 's/ *$//g')"
      chassis="$(dmidecode -s chassis-type 2>/dev/null | sed 's/ *$//g')"
      system="${vendor}; ${product}; v${version} (${chassis})"
      name_val "System" "${system}";
      servicetag="$(dmidecode -s system-serial-number 2>/dev/null | sed 's/ *$//g')"
      name_val "Service Tag" "${servicetag:-Not found}";
   fi

   # Try to find all sorts of different files that say what the release is.
   if [ -e /etc/fedora-release ]; then
      release=$(cat /etc/fedora-release);
   elif [ -e /etc/redhat-release ]; then
      release=$(cat /etc/redhat-release);
   elif [ -e /etc/system-release ]; then
      release=$(cat /etc/system-release);
   elif lsb_release -ds >/dev/null 2>&1; then
      release="$(lsb_release -ds) ($(lsb_release -cs))"
   elif [ -e /etc/lsb-release ]; then
      release=$(grep DISTRIB_DESCRIPTION /etc/lsb-release |awk -F'=' '{print $2}' |sed 's#"##g');
   elif [ -e /etc/debian_version ]; then
      release="Debian-based version $(cat /etc/debian_version)";
      if [ -e /etc/apt/sources.list ]; then
          code=`cat /etc/apt/sources.list |awk  '/^deb/ {print $3}' |awk -F/ '{print $1}'| awk 'BEGIN {FS="|"}{print $1}' | sort | uniq -c | sort -rn |head -n1 |awk '{print $2}'`
          release="${release} (${code})"
      fi
   elif ls /etc/*release 2>/dev/null; then
      if grep -q DISTRIB_DESCRIPTION /etc/*release; then
         release=$(grep DISTRIB_DESCRIPTION /etc/*release | head -n1);
      else
         release=$(cat /etc/*release | head -n1);
      fi
   fi
   name_val Release "${release}"
   name_val Kernel "$(uname -r)"

   CPU_ARCH='32-bit'
   if grep -q ' lm ' /proc/cpuinfo; then
      CPU_ARCH='64-bit'
   fi
   OS_ARCH='32-bit'
   if file -L /bin/sh | grep -q '64-bit'; then
      OS_ARCH='64-bit'
   fi
   name_val "Architecture" "CPU = $CPU_ARCH, OS = $OS_ARCH"

   # Threading library
   name_val Threading "$(getconf GNU_LIBPTHREAD_VERSION)"
   if [ -f /lib/libc.so.6 ]; then
      name_val "Compiler" "$(/lib/libc.so.6 | grep 'Compiled by' | cut -c13-)"
   fi

   if getenforce >/dev/null 2>&1; then
      getenforce="$(getenforce 2>&1)";
   fi
   name_val "SELinux" "${getenforce:-No SELinux detected}";

   # ########################################################################
   # Processor/CPU, Memory, Swappiness, dmidecode
   # ########################################################################
   section Processor
   cat /proc/cpuinfo > /tmp/aspersa
   parse_proc_cpuinfo

   section Memory
   free -b > /tmp/aspersa
   parse_free_minus_b

   rss=$(ps -eo rss | awk '/[0-9]/{total += $1 * 1024} END {print total}')
   shorten ${rss} rss
   name_val Used "${rss}"

   name_val Swappiness "$(sysctl vm.swappiness 2>&1)"
   name_val DirtyPolicy "$(sysctl vm.dirty_ratio 2>&1), $(sysctl vm.dirty_background_ratio 2>&1)"
   if sysctl vm.dirty_bytes > /dev/null 2>&1; then
      name_val DirtyStatus "$(sysctl vm.dirty_bytes 2>&1), $(sysctl vm.dirty_background_bytes 2>&1)"
   fi

   if dmidecode > /tmp/aspersa 2>/dev/null; then
      parse_dmidecode_mem_devices
   fi

   # ########################################################################
   # Disks, RAID, Filesystems
   # ########################################################################
   # TODO: Add info about software RAID

   if [[ "${ASPERSA_SKIP}" != *MOUNT* ]]; then
      section "Mounted_Filesystems"
      df -hP | sort > /tmp/aspersa2
      mount | sort | join /tmp/aspersa2 - > /tmp/aspersa
      parse_filesystems
   fi

   section "Disk_Schedulers_And_Queue_Size"

   echo "" > /tmp/aspersa
   for disk in $(ls /sys/block/ | grep -v ram | grep -v loop); do
      if [ -e "/sys/block/${disk}/queue/scheduler" ]; then
         name_val "${disk}" "$(cat /sys/block/${disk}/queue/scheduler | grep -o '\[.*\]') $(cat /sys/block/${disk}/queue/nr_requests)"
         fdisk -l "/dev/${disk}" >> /tmp/aspersa 2>/dev/null
      fi
   done

   # Relies on /tmp/aspersa having data from the Disk Schedulers loop.
   section "Disk_Partioning"
   parse_fdisk

   section "Kernel_Inode_State"
   for file in dentry-state file-nr inode-nr; do
      name_val "${file}" "$(cat /proc/sys/fs/${file} 2>&1)"
   done

   section "LVM_Volumes"

   if which lvs >/dev/null 2>&1 && test -x "$(which lvs)"; then
      lvs 2>&1
   else
      echo "Cannot execute 'lvs'";
   fi

   section "RAID_Controller"

   # ########################################################################
   # We look in lspci first because it's more reliable, then dmesg, because it's
   # often available to non-root users.  It's most reliable to look at
   # /var/log/dmesg if possible.
   # ########################################################################
   if lspci > /tmp/aspersa 2>/dev/null; then
      controller="$(parse_raid_controller_lspci)"
   fi
   if [ -z "${controller}" ]; then
      cat /var/log/dmesg > /tmp/aspersa 2>/dev/null
      if [ ! -s /tmp/aspersa ]; then
         dmesg > /tmp/aspersa 2>/dev/null
      fi
      controller="$(parse_raid_controller_dmesg)"
   fi

   name_val Controller "${controller:-No RAID controller detected}"

   # ########################################################################
   # Attempt to get, parse, and print RAID controller status from possibly
   # proprietary management software.  Any executables that are normally stored
   # in a weird location, such as /usr/StorMan/arcconf, should have their
   # location added to $PATH at the beginning of main().
   # ########################################################################
   notfound=""
   if [ "${controller}" = "AACRAID" ]; then
      if arcconf getconfig 1 > /tmp/aspersa 2>/dev/null; then
         parse_arcconf
      elif ! which arcconf >/dev/null 2>&1; then
         notfound="e.g. http://www.adaptec.com/en-US/support/raid/scsi_raid/ASR-2120S/"
      fi
   elif [ "${controller}" = "HP Smart Array" ]; then
      if hpacucli ctrl all show config > /tmp/aspersa 2>/dev/null; then
         parse_hpacucli
      elif ! which hpacucli >/dev/null 2>&1; then
         notfound="your package repository or the manufacturer's website"
      fi
   elif [ "${controller}" = "LSI Logic MegaRAID SAS" ]; then
      if MegaCli64 -AdpAllInfo -aALL > /tmp/aspersa 2>/dev/null; then
         parse_lsi_megaraid_adapter_info
      elif ! which MegaCli64 >/dev/null 2>&1; then
         notfound="your package repository or the manufacturer's website"
      fi
      if MegaCli64 -AdpBbuCmd -GetBbuStatus -aALL > /tmp/aspersa 2>/dev/null; then
         parse_lsi_megaraid_bbu_status
      fi
      if MegaCli64 -LdPdInfo -aALL > /tmp/aspersa 2>/dev/null; then
         parse_lsi_megaraid_virtual_devices
         parse_lsi_megaraid_devices
      fi
   fi

   if [ "${notfound}" ]; then
      echo "   RAID controller software not found; try getting it from"
      echo "   ${notfound}"
   fi

   # ########################################################################
   # We look in dmesg for virtualization information first, because it's often
   # available to non-root users and usually has telltale signs.  It's most
   # reliable to look at /var/log/dmesg if possible.  There are a number of
   # other ways to find out if a system is virtualized.
   # ########################################################################
   section Virtualization
   cat /var/log/dmesg > /tmp/aspersa 2>/dev/null
   if [ ! -s /tmp/aspersa ]; then
      dmesg > /tmp/aspersa 2>/dev/null
   fi
   if [ -s /tmp/aspersa ]; then
      virt="$(parse_virtualization_dmesg)"
   fi
   if [ -z "${virt}" ]; then
      lspci > /tmp/aspersa 2>/dev/null
      if grep -qi virtualbox /tmp/aspersa; then
         virt=VirtualBox
      elif grep -qi vmware /tmp/aspersa; then
         virt=VMWare
      elif [ -e /proc/user_beancounters ]; then
         virt="OpenVZ/Virtuozzo"
      fi
   fi
   name_val Technology "${virt:-No virtualization detected}"

   if [[ "${ASPERSA_SKIP}" != *NETWORK* ]]; then
      # #####################################################################
      # Network stuff
      # #####################################################################
      section Network_Config
      if lspci > /tmp/aspersa 2>/dev/null; then
         parse_ethernet_controller_lspci
      fi
      if sysctl net.ipv4.tcp_fin_timeout > /dev/null 2>&1; then
         name_val "FIN Timeout" "$(sysctl net.ipv4.tcp_fin_timeout)"
         name_val "Port Range" "$(sysctl net.ipv4.ip_local_port_range)"
      fi

      # TODO cat /proc/sys/net/ipv4/ip_conntrack_max ; it might be
      # /proc/sys/net/netfilter/nf_conntrack_max or /proc/sys/net/nf_conntrack_max
      # in new kernels like Fedora 12?

       
      # TODO: use fuzzy-rounding
      section Interface_Statistics
      if ip -s link > /tmp/aspersa 2>/dev/null; then
         parse_ip_s_link
      fi

      section Network_Connections
      if netstat -antp > /tmp/aspersa 2>/dev/null; then
         parse_netstat
      fi
   fi

   if [[ "${ASPERSA_SKIP}" != *PROCESS* ]]; then
      # ########################################################################
      # Processes, load, etc
      # ########################################################################
      section Top_Processes
      top -bn 1 | sed -e 's# *$##g' -e '/./{H;$!d;}' -e 'x;/PID/!d;' | grep . | head
      section "Simplified_vmstat_(wait_please)"
      vmstat 1 5 > /tmp/aspersa
      format_vmstat
   fi

   # ########################################################################
   # All done.  Signal the end so it's explicit.
   # ########################################################################
   temp_files "rm"
   temp_files "check"
   section The_End
}

# Execute the program if it was not included from another file.  This makes it
# possible to include without executing, and thus test.
#if [ $(basename "$0") = "summary" ] || [ $(basename "$0") = "bash" -a "$_" = "$0" ]; then
    main $*
#fi
