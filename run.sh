#!/bin/bash

if [[ "$1" == "clean" ]]; then
    rm -rf classes
else
    mkdir -p classes
    scalac -sourcepath src -d classes src/alife/*
    scala -cp classes alife.Main
fi
