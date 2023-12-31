module Top where 

import Clash.Prelude
import qualified UDP 
import qualified ConstantStreamer
import qualified UdpCounters

system :: HiddenClockResetEnable dom => UDP.UdpTop dom ->
    Signal dom (UDP.UdpRxIn, UDP.UdpTxIn) -> Signal dom (UDP.UdpRxOut, UDP.UdpTxOut)
system udp_top inputs = outputs
    where
        (udp_rx_in, udp_tx_in) = unbundle inputs
        udp_tx_in_d = register def udp_tx_in
        -- TODO: Better generic top type, make it easy to interchange business logic
        (udp_rx_out, udp_tx_out) = udp_top udp_rx_in udp_tx_in
        --
        udp_tx_out_d = register def udp_tx_out 
        outputs = bundle (udp_rx_out, udp_tx_out_d)

{-# ANN topEntity
  (Synthesize
    { t_name   = "main"
    , t_inputs = [ PortName "clock"
                 , PortName "reset"
                 , PortName "enable"
                 , PortProduct "udp" [PortProduct "rx" [PortName "valid", PortName "first", PortName "last", PortName "payload", PortName "last_be"], PortName "tx_ready"]]
    , t_output = PortProduct "udp" [PortName "rx_ready", PortProduct "tx" [PortName "valid", PortName "first", PortName "last", PortName "payload", PortName "last_be"]]
    }) #-}
topEntity ::
    Clock System
     -> Reset System
     -> Enable System
     -> Signal System (UDP.UdpRxIn, UDP.UdpTxIn)
     -> Signal System (UDP.UdpRxOut, UDP.UdpTxOut)
topEntity = exposeClockResetEnable (system UdpCounters.asTop)