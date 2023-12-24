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
from common import clash, ip_hacks

from top import NAFTop


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

class Platform(SimPlatform):
    def __init__(self):
        SimPlatform.__init__(self, "SIM", _io)

class NAFSim(SoCMini):
    def __init__(self, sys_clk_freq=int(40e6), ip_address=None, mac_address=None, manager_ip=None):
        platform = Platform()
        platform.add_source_dir("../clash/verilog/")

        # CRG --------------------
        self.crg = CRG(platform.request("sys_clk"))

        # SoCMini --------------------
        SoCMini.__init__(self, platform, clk_freq=sys_clk_freq)

        # Ethernet --------------------
        self.ethphy = ethphy = LiteEthPHYModel(self.platform.request("eth"))

        data_width = 32

        self.ip_udp_core = ip_udp_core = LiteEthUDPIPCore(
            phy         = self.ethphy,
            mac_address = mac_address,
            ip_address  = ip_address,
            clk_freq    = self.clk_freq
        )

        self.top = NAFTop(data_width, ip_udp_core, manager_ip)


def main():
    parser = argparse.ArgumentParser(description="Network Attached FPGA Simulation")
    builder_args(parser)
    parser.add_argument("--threads",          default=1,              help="Set number of threads (default=1)")
    parser.add_argument("--trace",            action="store_true",    help="Enable VCD tracing")
    parser.add_argument("--trace-start",      default=0,              help="Cycle to start VCD tracing")
    parser.add_argument("--trace-end",        default=-1,             help="Cycle to end VCD tracing")
    parser.add_argument("--opt-level",        default="O0",           help="Compilation optimization level")
    parser.add_argument("--manager-ip",       default=ip_hacks.probable_ip(),  help="IP address of machine running a NAF manager program.")
    parser.add_argument("--ip-address",       default="172.30.28.50",   help="Ethernet IP address of the board (default: 192.168.1.20).")
    parser.add_argument("--mac-address",      default="0x726b895bc2e2", help="Ethernet MAC address of the board (defaullt: 0x726b895bc2e2).")
    
    args = parser.parse_args()

    soc_kwargs     = {}
    builder_kwargs = builder_argdict(args)

    sim_config = SimConfig(default_clk="sys_clk")
    sim_config.add_module("ethernet", "eth", args={"interface": "tap0", "ip": args.ip_address})

    clash.build()

    soc = NAFSim(ip_address=args.ip_address, mac_address=int(args.mac_address, 0), manager_ip=args.manager_ip, **soc_kwargs)
    builder = Builder(soc, csr_csv="scripts/csr.csv")
    vns = builder.build(threads=args.threads, sim_config=sim_config,
        opt_level   = args.opt_level,
        trace       = args.trace,
        trace_start = int(args.trace_start),
        trace_end   = int(args.trace_end)
    )

if __name__ == "__main__":
    main()