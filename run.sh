#!/bin/bash

if [[ "$1" == "clean" ]]; then
    rm -rf classes
    rm -rf bacteria.jar
elif [[ "$1" == "jar" ]]; then
    java -Dscala.usejavacp=true -cp 'lib/*' scala.tools.nsc.Main -deprecation -sourcepath src -d bacteria.jar src/alife/*.scala src/alife/sound/*.scala
elif [[ "$1" == "run" ]]; then
    java -Xmx1G -cp lib/scala-library-2.13.2.jar:bacteria.jar alife.Main
else
    mkdir -p classes
    scalac -sourcepath src -d classes src/alife/*.scala src/alife/sound/*.scala
    scala -J-Xmx1G -cp classes alife.Main
fi
