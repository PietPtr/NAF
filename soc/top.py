#!/usr/bin/env python3

#
# This file is part of Colorlite.
#
# Copyright (c) 2020-2022 Florent Kermarrec <florent@enjoy-digital.fr>
# SPDX-License-Identifier: BSD-2-Clause

import os
import argparse
import sys
from liteeth.core import LiteEthUDPIPCore
from liteeth.core.arp import LiteEthARP
from liteeth.core.udp import LiteEthUDP
from liteeth.phy.model import LiteEthPHYModel
from liteeth.phy.rmii import LiteEthPHYRMII

from migen import *
from migen.genlib.misc import WaitTimer
from migen.genlib.resetsync import AsyncResetSynchronizer

from litex.gen import *

from litex_boards.platforms import colorlight_5a_75b

from litex.soc.cores.clock import *
from litex.soc.cores.spi_flash import ECP5SPIFlash
from litex.soc.cores.gpio import GPIOOut
from litex.soc.cores.led import LedChaser
from litex.soc.integration.soc_core import *
from litex.soc.integration.builder import *

from liteeth.phy.ecp5rgmii import LiteEthPHYRGMII
from liteeth.core.ip import LiteEthIP, LiteEthIPTX
from litex.build.generic_platform import *
from liteeth.common import convert_ip, eth_udp_user_description

from litex.soc.interconnect.packet import *

# Network Attached FPGA --------------------

class NAFTop(LiteXModule):
    def __init__(self, data_width, ip_udp_core, manager_ip):
        udp_listen_port = 50059
        raw_port = ip_udp_core.udp.crossbar.get_port(udp_listen_port)

        self.in_fifo = in_fifo = PacketFIFO(eth_udp_user_description(data_width),
            payload_depth = 32,
            param_depth = 4,
            buffered = True
        )

        self.out_fifo = out_fifo = PacketFIFO(eth_udp_user_description(data_width),
            payload_depth = 32,
            param_depth = 4,
            buffered = True
        )

        self.comb += [
            raw_port.source.connect(in_fifo.sink, omit = {"src_port", "dst_port"}),
            in_fifo.sink.src_port.eq(raw_port.source.dst_port),
            in_fifo.sink.dst_port.eq(raw_port.source.src_port),

            out_fifo.source.connect(raw_port.sink),
            out_fifo.sink.param.ip_address.eq(convert_ip(manager_ip)),
            out_fifo.sink.param.length.eq(21), # TODO: expose length and ports to clash
            out_fifo.sink.param.src_port.eq(udp_listen_port),
            out_fifo.sink.param.dst_port.eq(50060),
        ]

        self.specials += Instance("boilerplate",
            i_clk=ClockSignal("sys"),
            i_rst=ResetSignal("sys"),

            i_valid_rx=in_fifo.source.valid,
            o_ready_rx=in_fifo.source.ready,
            i_first_rx=in_fifo.source.first,
            i_last_rx=in_fifo.source.last,
            i_payload_rx=in_fifo.source.payload.data,
            i_last_be_rx=in_fifo.source.payload.last_be,

            o_valid_tx=out_fifo.sink.valid,
            i_ready_tx=out_fifo.sink.ready,
            o_first_tx=out_fifo.sink.first,
            o_last_tx=out_fifo.sink.last,
            o_payload_tx=out_fifo.sink.payload.data,
            o_last_be_tx=out_fifo.sink.payload.last_be,
        )

# Build --------------------

def main():
    parser = argparse.ArgumentParser(description="Take control of your ColorLight FPGA board with LiteX/LiteEth :)")
    parser.add_argument("--build",       action="store_true",      help="Build bitstream")
    parser.add_argument("--load",        action="store_true",      help="Load bitstream")
    parser.add_argument("--flash",       action="store_true",      help="Flash bitstream")
    parser.add_argument("--ip-address",  default="192.168.1.20",   help="Ethernet IP address of the board (default: 192.168.1.20).")
    parser.add_argument("--mac-address", default="0x726b895bc2e2", help="Ethernet MAC address of the board (defaullt: 0x726b895bc2e2).")
    parser.add_argument("--manager-ip",  default="192.168.1.1",    help="IP address of the server using the board as computation device.")
    args = parser.parse_args()

    # TODO: call clash here to generate verilog

    soc     = NAF(ip_address=args.ip_address, mac_address=int(args.mac_address, 0), manager_ip=args.manager_ip)
    builder = Builder(soc, output_dir="build", csr_csv="scripts/csr.csv")
    builder.build(build_name="naf", run=args.build)

    if args.load:
        prog = soc.platform.create_programmer()
        prog.load_bitstream(os.path.join(builder.gateware_dir, soc.build_name + ".svf"))

    if args.flash:
        prog = soc.platform.create_programmer()
        # os.system("cp bit_to_flash.py build/gateware/")
        os.system("cd build/gateware && chmod +x ./build_naf.sh && ./build_naf.sh")
        prog.load_bitstream(os.path.join(builder.gateware_dir, soc.build_name + ".bit"))

if __name__ == "__main__":
    main()
