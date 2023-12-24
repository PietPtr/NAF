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
