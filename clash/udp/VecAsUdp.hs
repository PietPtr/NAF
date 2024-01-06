{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module VecAsUdp (asTop, sendVec) where

import Clash.Prelude

import UDP
import Data.Maybe
import Data.Proxy
import Debug.Trace
import Transactions
import Comps

import qualified Data.List as L

asTop :: HiddenClockResetEnable dom => UdpTop dom
asTop _ udp_tx_in = ((pure $ UdpRxOut {rx_ready = 1}), udp_tx_out)
    where
        to_send_vector = constJustPulser @4096 (0xde :> 0xad :> 0xbe :> 0xef :> 0x00 :> 0xab :> 0xc9 :> 0xee :> Nil)
        udp_tx_out = sendVec udp_tx_in to_send_vector

sendVec :: (KnownNat n, HiddenClockResetEnable dom) 
    => Signal dom (UdpTxIn) -> Signal dom (Maybe (Vec n (BitVector 8)))
    -> Signal dom (UdpTxOut)
sendVec udp_tx_in vec_to_send = (mealy sendVec' def) (bundle (udp_tx_in, vec_to_send))

data SendVecState n = SendVecState {
    sender_state :: SendState n,
    payload :: Vec n (BitVector 8)
} deriving (Default, Generic, NFDataX, Show)

sendVec' :: forall n . (KnownNat n) => SendVecState n -> (UdpTxIn, Maybe (Vec n (BitVector 8))) 
    -> (SendVecState n, UdpTxOut)
sendVec' (SendVecState {..}) (UdpTxIn {..}, vector) = (state', udp_out)
    where
        vec_width = maxBound :: Index n

        sender_state' = case sender_state of
            Idle -> if isJust vector then (Sending 0) else Idle
            Sending n -> case (n == vec_width, tx_ready == 1) of
                (True, True) -> Idle
                (False, True) -> Sending (n + 1)
                (_, False) -> Sending n
        
        payload' = case vector of
            (Just v) -> v
            Nothing -> payload

        udp_out = UdpTxOut {
            tx_valid = bitCoerce $ sender_state /= Idle,
            tx_first = bitCoerce $ sender_state == Sending 0,
            tx_last = bitCoerce $ sender_state == Sending vec_width,
            tx_payload = case sender_state of -- TODO: error in ready handling
                Sending n -> payload !! n 
                Idle -> 0xff
        }

        state' = SendVecState {
            sender_state = sender_state',
            payload = payload'
        }

testInput :: [UdpTxIn]
testInput = L.map UdpTxIn [0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1]

deadbeefStream = L.zip testInput (simulate @System dut testInput)
    where
        dut readys = sendVec readys (pure $ Just $ 0 :> 1 :> 2 :> 3 :> 4 :> 5 :> 6 :> 7 :> 8 :> Nil)

test1 = filterTransactions deadbeefStream