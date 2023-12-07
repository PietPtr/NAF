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
from litex.soc.interconnect import wishbone

from liteeth.phy.ecp5rgmii import LiteEthPHYRGMII
from liteeth.core.ip import LiteEthIP, LiteEthIPTX
from litex.build.generic_platform import *
from liteeth.common import convert_ip




# CRG --------------------

class _CRG(LiteXModule):
    def __init__(self, platform, sys_clk_freq):
        self.cd_sys = ClockDomain()
        # # #

        # Clk / Rst.
        clk25 = platform.request("clk25")
        rst_n = platform.request("user_btn_n", 0)

        # PLL.
        self.pll = pll = ECP5PLL()
        self.comb += pll.reset.eq(~rst_n)
        pll.register_clkin(clk25, 25e6)
        pll.create_clkout(self.cd_sys, sys_clk_freq)

# Clash Wrapper --------------------

class ClashWrapper(Module):
    def __init__(self, platform):
        # Wishbone Interface
        self.bus = bus = wishbone.Interface()

        # Verilog Module Instance
        self.specials += Instance("ClashWrapper",
            # Clock and Reset
            i_clk = ClockSignal(),
            i_rst = ResetSignal(),

            # Wishbone Interface
            i_adr   = bus.adr,
            i_dat_w = bus.dat_w,
            o_dat_r = bus.dat_r,
            i_we    = bus.we,
            i_stb   = bus.stb,
            o_ack   = bus.ack,
            i_sel   = bus.sel,
            i_cyc   = bus.cyc
        )


# ColorLite --------------------

class ColorLite(SoCMini):
    def __init__(self, sys_clk_freq=int(40e6), with_etherbone=True, ip_address=None, mac_address=None):
        platform = colorlight_5a_75b.Platform(revision="7.0")
        platform.add_source_dir("../clash/verilog/")

        # CRG --------------------
        self.crg = _CRG(platform, sys_clk_freq)

        # SoCMini --------------------
        SoCMini.__init__(self, platform, clk_freq=sys_clk_freq)

        # Ethernet --------------------
        self.ethphy = LiteEthPHYRGMII(
            clock_pads = self.platform.request("eth_clocks"),
            pads = self.platform.request("eth"),
            tx_delay = 0e-9)
        
        eth_dw = 8

        self.add_ethernet(
            phy = self.ethphy,
            phy_cd = self.crg,
            data_width = eth_dw
        )

        self.ethcore = LiteEthUDPIPCore(
            phy         = self.ethphy,
            mac_address = mac_address,
            ip_address  = ip_address,
            clk_freq    = self.clk_freq,
            arp_entries = 1,
            dw          = eth_dw,
            with_ip_broadcast = True,
            with_sys_datapath = True,
            interface   = {True :            "hybrid", False: "crossbar"}[with_ethmac],
            endianness  = {True : self.cpu.endianness, False:      "big"}[with_ethmac],
        )

        # Led --------------------
        self.leds = LedChaser(
            pads         = platform.request_all("user_led_n"),
            sys_clk_freq = sys_clk_freq)
        
        # Custom hardware
        # din = Signal(33)
        # dout = Signal(33)
        # self.specials = Instance("clash_wrapper",
        #     data_in = din,
        #     data_out = dout
        # )


# Build --------------------

def main():
    parser = argparse.ArgumentParser(description="Take control of your ColorLight FPGA board with LiteX/LiteEth :)")
    parser.add_argument("--build",       action="store_true",      help="Build bitstream")
    parser.add_argument("--load",        action="store_true",      help="Load bitstream")
    parser.add_argument("--flash",       action="store_true",      help="Flash bitstream")
    parser.add_argument("--ip-address",  default="192.168.1.20",   help="Ethernet IP address of the board (default: 192.168.1.20).")
    parser.add_argument("--mac-address", default="0x726b895bc2e2", help="Ethernet MAC address of the board (defaullt: 0x726b895bc2e2).")
    args = parser.parse_args()

    # TODO: call clash here to generate SV

    soc     = ColorLite(ip_address=args.ip_address, mac_address=int(args.mac_address, 0))
    builder = Builder(soc, output_dir="build", csr_csv="scripts/csr.csv")
    builder.build(build_name="colorlite", run=args.build)

    if args.load:
        prog = soc.platform.create_programmer()
        prog.load_bitstream(os.path.join(builder.gateware_dir, soc.build_name + ".svf"))

    if args.flash:
        prog = soc.platform.create_programmer()
        os.system("cp bit_to_flash.py build/gateware/")
        os.system("cd build/gateware && ./bit_to_flash.py colorlite.bit colorlite.svf.flash")
        prog.load_bitstream(os.path.join(builder.gateware_dir, soc.build_name + ".svf.flash"))

if __name__ == "__main__":
    main()
