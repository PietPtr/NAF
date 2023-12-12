{-# LANGUAGE NamedFieldPuns #-}
module UDP where

import Clash.Prelude hiding (last)

data UdpTxIn = UdpTxIn {
    tx_ready :: Bit
}

data UdpTxOut = UdpTxOut {
    tx_valid :: Bit,
    tx_first :: Bit,
    tx_last :: Bit,
    tx_payload :: BitVector 32,
    tx_last_be :: BitVector 4
}

data UdpRxIn = UdpRxIn {
    rx_valid :: Bit,
    rx_first :: Bit,
    rx_last :: Bit,
    rx_payload :: BitVector 32,
    rx_last_be :: BitVector 4
}

data UdpRxOut = UdpRxOut {
    rx_ready :: Bit
}

constantUdpStreamer :: HiddenClockResetEnable dom =>
    Signal dom UdpTxIn -> Signal dom UdpTxOut
constantUdpStreamer = mealy constantUdpStreamer' ()

data ConstantUdpStreamerState = ConstantUdpStreamerState {
    payload_counter :: Unsigned 8
} deriving (Default)

constantUdpStreamer' :: ConstantUdpStreamerState -> UdpTxIn -> (ConstantUdpStreamerState, UdpTxOut)
constantUdpStreamer' state (UdpTxIn { tx_ready }) = (state, out)
    where
        out = UdpTxOut {
            valid = 0,
            first = 0,
            last = 0,
            payload = 0,
            last_be = 0
        }

