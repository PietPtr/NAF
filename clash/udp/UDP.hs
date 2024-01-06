{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
module UDP where

import Transactions
import Clash.Prelude hiding (last, (++))

type UdpTop dom = 
    Signal dom (UDP.UdpRxIn) -> Signal dom (UDP.UdpTxIn) ->
    (Signal dom (UDP.UdpRxOut), Signal dom (UDP.UdpTxOut))

data UdpTxIn = UdpTxIn {
    tx_ready :: Bit
} deriving (Generic, NFDataX, Default, Show)

data UdpTxOut = UdpTxOut {
    tx_valid :: Bit,
    tx_first :: Bit,
    tx_last :: Bit,
    tx_payload :: BitVector 8
} deriving (Generic, NFDataX, Default, Show)

instance Transaction UdpTxIn UdpTxOut where
    didTransact (UdpTxIn {..}) (UdpTxOut {..}) = tx_ready .&. tx_valid

data UdpRxIn = UdpRxIn {
    rx_valid :: Bit,
    rx_first :: Bit,
    rx_last :: Bit,
    rx_payload :: BitVector 8,
    rx_last_be :: Bit
} deriving (Generic, NFDataX, Default, Show)

data UdpRxOut = UdpRxOut {
    rx_ready :: Bit
} deriving (Generic, NFDataX, Default, Show)

instance Transaction UdpRxIn UdpRxOut where
    didTransact (UdpRxIn {..}) (UdpRxOut {..}) = rx_ready .&. rx_valid

data SendState n = Idle | Sending (Index n)
    deriving (Generic, NFDataX, Show, Eq)

instance Default (SendState n) where
    def = Idle

combineRxReady :: UdpRxOut -> UdpRxOut -> UdpRxOut
combineRxReady (UdpRxOut { rx_ready = ready1 }) (UdpRxOut { rx_ready = ready2 }) 
    = UdpRxOut { rx_ready = ready1 .|. ready2 }