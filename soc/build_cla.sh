#!/bin/bash
clash ../clash/Top.hs \
    -i../clash/ \
    -i../clash/udp/ \
    -i../clash/common/ \
    --verilog -outputdir ../clash/verilog/