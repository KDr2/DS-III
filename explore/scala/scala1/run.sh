#!/bin/sh
if [ X$M2_HOME = X ]; then
export M2_HOME=/home/kdr2/programs/maven-3.2.1
fi
$M2_HOME/bin/mvn scala:run -DmainClass=net.kdr2.scala0.Runner
