#!/bin/bash

set -ex

gcc -v

g++ -v

cat > test.cpp <<EOF
#include <vector>
int main(){}
EOF

g++ test.cpp

dpkg -l | grep libstdc

apt-cache search libstdc++
