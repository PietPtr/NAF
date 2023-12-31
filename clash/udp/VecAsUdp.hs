{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
module VecAsUdp where

import Clash.Prelude

import UDP
import Data.Maybe
import Data.Proxy

sendVec :: (KnownNat n, HiddenClockResetEnable dom) 
    => Signal dom (UdpTxIn) -> Signal dom (Maybe (Vec n (BitVector 8)))
    -> Signal dom (UdpTxOut)
sendVec udp_tx_in vec_to_send = (mealy sendVec' def) (bundle (udp_tx_in, vec_to_send))

data SendVecState n = SendVecState {
    sender_state :: SendState n,
    payload :: Vec n (BitVector 8)
} deriving (Default, Generic, NFDataX, Show)

sendVec' :: forall n. (KnownNat n) => SendVecState n -> (UdpTxIn, Maybe (Vec n (BitVector 8))) 
    -> (SendVecState n, UdpTxOut)
sendVec' (SendVecState {..}) (UdpTxIn {..}, vector) = (state', udp_out)
    where
        vec_width = fromIntegral (natVal (Proxy @n)) :: Index n

        sender_state' = case sender_state of
            Idle -> if isJust vector then Sending vec_width else Idle
            Sending 0 -> Idle
            Sending n -> if tx_ready == 1 
                then Sending (n - 1) 
                else Sending n
        
        payload' = case vector of
            (Just v) -> v
            Nothing -> payload

        udp_out = UdpTxOut {
            tx_valid = bitCoerce $ sender_state /= Idle,
            tx_first = bitCoerce $ sender_state == Sending vec_width,
            tx_last = bitCoerce $ sender_state == Sending 0,
            tx_payload = case sender_state of
                Sending n -> payload !! n 
                Idle -> 0xff
        }

        state' = SendVecState {
            sender_state = sender_state',
            payload = payload'
        }
