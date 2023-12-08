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
from litex.soc.interconnect import wishbone

from liteeth.phy.ecp5rgmii import LiteEthPHYRGMII
from liteeth.core.ip import LiteEthIP, LiteEthIPTX
from litex.build.generic_platform import *
from liteeth.common import convert_ip, eth_udp_user_description

from litex.soc.interconnect.packet import *



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


# UDP to App --------------------

class UdpToApp(LiteXModule):
    def __init__(self, udp_in_port, udp_out_port):
        self.sink   = sink   = stream.Endpoint(eth_udp_user_description(32))
        self.source = source = stream.Endpoint(eth_udp_user_description(32))




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

        self.ethphy = phy = LiteEthPHYRGMII(
            clock_pads = self.platform.request("eth_clocks"),
            pads = self.platform.request("eth"),
            tx_delay = 0e-9)
        
        eth_dw = 32

        # self.add_ethernet(
        #     phy = self.ethphy,
        #     phy_cd = self.crg,
        #     data_width = eth_dw
        # )

        ethcore = LiteEthUDPIPCore(
            phy         = self.ethphy,
            mac_address = mac_address,
            ip_address  = ip_address,
            clk_freq    = self.clk_freq,
            arp_entries = 1,
            dw          = eth_dw,
            with_ip_broadcast = True,
            with_sys_datapath = True,
            interface   = "crossbar",
            endianness  = "big"
        )


        app_port = ethcore.udp.crossbar.get_port(50059)
        # self.comb += app_port.source.param.src_port.eq(0xdeadbeef)
        print(app_port.source.payload)
        print(app_port.sink)
        # self.comb += app_port.source.
        self.specials += Instance("boilerplate",
            i_valid_rx=app_port.sink.valid,
            o_ready_rx=app_port.sink.ready,
            i_first_rx=app_port.sink.first,
            i_last_rx=app_port.sink.last,
            i_payload_rx=app_port.sink.payload.data,

            o_valid_tx=app_port.source.valid,
            i_ready_tx=app_port.source.ready,
            o_first_tx=app_port.source.first,
            o_last_tx=app_port.source.last,
            o_payload_tx=app_port.source.payload.data
        )

        self.add_module(name=f"ethcore_udp", module=ethcore)

        eth_rx_clk = getattr(phy, "crg", phy).cd_eth_rx.clk
        eth_tx_clk = getattr(phy, "crg", phy).cd_eth_tx.clk
        if not isinstance(phy, LiteEthPHYModel) and not getattr(phy, "model", False):
            print("not lite eth phy model")
            self.platform.add_period_constraint(eth_rx_clk, 1e9/phy.rx_clk_freq)
            if not eth_rx_clk is eth_tx_clk:
                print("rx clk =/= tx clk")
                self.platform.add_period_constraint(eth_tx_clk, 1e9/phy.tx_clk_freq)
                self.platform.add_false_path_constraints(self.crg.cd_sys.clk, eth_rx_clk, eth_tx_clk)
            else:
                print("rx clk = tx clk")
                self.platform.add_false_path_constraints(self.crg.cd_sys.clk, eth_rx_clk)

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
