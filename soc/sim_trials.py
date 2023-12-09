#!/usr/bin/env python3

# This file is Copyright (c) 2020 Florent Kermarrec <florent@enjoy-digital.fr>
# License: BSD

import argparse

from migen import *
from migen.genlib.cdc import *

from litex.build.generic_platform import *
from litex.build.sim import SimPlatform
from litex.build.sim.config import SimConfig

from litex.soc.integration.soc_core import *
from litex.soc.integration.builder import *
from litex.soc.cores import uart
from litex.soc.interconnect import stream
from litex.soc.interconnect.packet import *

from liteeth.phy.model import LiteEthPHYModel
from liteeth.core import LiteEthUDPIPCore
from liteeth.common import convert_ip, eth_udp_user_description


_io = [
    ("sys_clk", 0, Pins(1)),
    ("sys_rst", 0, Pins(1)),
    ("eth_clocks", 0,
        Subsignal("tx", Pins(1)),
        Subsignal("rx", Pins(1)),
    ),
    ("eth", 0,
        Subsignal("source_valid", Pins(1)),
        Subsignal("source_ready", Pins(1)),
        Subsignal("source_data",  Pins(8)),

        Subsignal("sink_valid",   Pins(1)),
        Subsignal("sink_ready",   Pins(1)),
        Subsignal("sink_data",    Pins(8)),
    ),
]

class UDPPacketStreamer(Module):
    def __init__(self, fifo):
        self.fifo = fifo

        # Packet generation variables
        self.local_ip = "172.30.28.50"
        self.remote_ip = "192.168.2.5"
        self.local_port = 50059
        self.remote_port = 50059
        self.packet_length = 10

        # Control logic for packet streaming
        TIMER_MAX = 100000
        self.timer = Signal(max=TIMER_MAX)  # Adjust the max value for your timer interval

        self.counter = Signal(max=self.packet_length)
        self.packet_ready = Signal()

        self.sync += [
            fifo.sink.param.ip_address.eq(convert_ip(self.remote_ip)),
            fifo.sink.param.src_port.eq(self.local_port),
            fifo.sink.param.dst_port.eq(self.remote_port),
            fifo.sink.param.length.eq(self.packet_length),

            self.timer.eq(self.timer + 1),
            If(self.packet_ready,
                If(fifo.sink.ready,
                    fifo.sink.valid.eq(1),
                    fifo.sink.last.eq(self.counter == self.packet_length - 1),
                    fifo.sink.payload.data.eq(0x01234567),
                    fifo.sink.payload.last_be.eq(0b1111),
                    fifo.sink.payload.error.eq(0b0000),
                    If(fifo.sink.valid & fifo.sink.ready,
                        self.counter.eq(self.counter + 1),
                        If(self.counter == self.packet_length - 1,
                            self.counter.eq(0),
                            self.packet_ready.eq(0),
                            self.timer.eq(0)  # Reset the timer after sending a packet
                        )
                    )
                )
            ).Else(
                If(self.timer == TIMER_MAX - 1, 
                    self.packet_ready.eq(1)
                )
            )
        ]

class Platform(SimPlatform):
    def __init__(self):
        SimPlatform.__init__(self, "SIM", _io)

class ColorLite(SoCMini):
    def __init__(self, sys_clk_freq=int(40e6)):
        platform = Platform()

        # CRG --------------------
        self.crg = CRG(platform.request("sys_clk"))

        # SoCMini --------------------
        SoCMini.__init__(self, platform, clk_freq=sys_clk_freq)

        # Ethernet --------------------
        self.ethphy = ethphy = LiteEthPHYModel(self.platform.request("eth"))

        data_width = 32

        self.ip_udp_core = core = LiteEthUDPIPCore(
            phy         = self.ethphy,
            mac_address = 0x10e2d4000000,
            ip_address  = "172.30.28.50",
            clk_freq    = self.clk_freq
        )

        udp_listen_port = 50059
        raw_port = self.ip_udp_core.udp.crossbar.get_port(udp_listen_port)

        self.fifo = fifo = PacketFIFO(eth_udp_user_description(data_width),
            payload_depth = 40,
            param_depth   = 4,
            buffered      = True
        )

        self.udp_streamer = UDPPacketStreamer(fifo=fifo)

        print(self.fifo.sink.payload.error.nbits)

        self.comb += [
            # raw_port.source.connect(fifo.sink, omit = {"src_port", "dst_port"}),
            # fifo.sink.src_port.eq(raw_port.source.dst_port),
            # fifo.sink.dst_port.eq(raw_port.source.src_port),
            fifo.source.connect(raw_port.sink)
        ]

def main():
    parser = argparse.ArgumentParser(description="Network Attached FPGA Simulation")
    builder_args(parser)
    parser.add_argument("--threads",          default=1,              help="Set number of threads (default=1)")
    parser.add_argument("--trace",            action="store_true",    help="Enable VCD tracing")
    parser.add_argument("--trace-start",      default=0,              help="Cycle to start VCD tracing")
    parser.add_argument("--trace-end",        default=-1,             help="Cycle to end VCD tracing")
    parser.add_argument("--opt-level",        default="O0",           help="Compilation optimization level")
    args = parser.parse_args()

    soc_kwargs     = {}
    builder_kwargs = builder_argdict(args)

    sim_config = SimConfig(default_clk="sys_clk")

    sim_config.add_module("ethernet", "eth", args={"interface": "tap0", "ip": "172.30.28.50"})

    soc = ColorLite(**soc_kwargs)
    builder = Builder(soc)
    vns = builder.build(threads=args.threads, sim_config=sim_config,
        opt_level   = args.opt_level,
        trace       = args.trace,
        trace_start = int(args.trace_start),
        trace_end   = int(args.trace_end)
    )

if __name__ == "__main__":
    main()