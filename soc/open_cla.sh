#!/bin/bash
if [ -z "$1" ]; then
    FILE="Top.hs"
else
    FILE="$1"
fi

clash --interactive ../clash/$FILE -i../clash/ -i../clash/udp/ -i../clash/libtest/
