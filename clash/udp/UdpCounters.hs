{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
module UdpCounters (asTop, packetCounter, payloadCounter) where

import Clash.Prelude

import UDP hiding (Idle)
import qualified VecAsUdp
import qualified Data.List as L

import Debug.Trace

asTop :: HiddenClockResetEnable dom => UdpTop dom
asTop udp_rx_in udp_tx_in = 
    ( UDP.combineRxReady <$> packet_udp_rx_out <*> payload_udp_rx_out
    , udp_tx_out)
    where
        (packet_udp_rx_out, packet_count) = unbundle $ packetCounter udp_rx_in
        (payload_udp_rx_out, payload_count) = unbundle $ payloadCounter udp_rx_in

        udp_tx_out = VecAsUdp.sendVec @8 udp_tx_in (f <$> udp_rx_in <*> packet_count <*> payload_count)
        
        f udp_rx_in packet_count payload_count = 
            if (rx_first udp_rx_in) .&. (rx_valid udp_rx_in) == 1 
                then Just (unpack $ resize (pack packet_count ++# pack payload_count))
                else Nothing


packetCounter :: HiddenClockResetEnable dom => Signal dom UdpRxIn 
    -> Signal dom (UdpRxOut, Unsigned 32)
packetCounter = mealy packetCounter' def

packetCounter' :: Unsigned 32 -> UdpRxIn -> (Unsigned 32, (UdpRxOut, Unsigned 32))
packetCounter' state (UdpRxIn {..}) = (state', (udp_out, state))
    where
        udp_out = UdpRxOut { rx_ready = 1 }
        state' = if (rx_first .&. rx_valid) == 1 
            then state + 1 
            else state

payloadCounter :: HiddenClockResetEnable dom => Signal dom UdpRxIn 
    -> Signal dom (UdpRxOut, Unsigned 32)
payloadCounter = mealy payloadCounter' def

data PayloadCounterState = Idle | Receiving
    deriving (Show, Generic, NFDataX)

instance Default PayloadCounterState where
    def = Idle

payloadCounter' :: (Unsigned 32, PayloadCounterState) -> UdpRxIn 
    -> ((Unsigned 32, PayloadCounterState), (UdpRxOut, Unsigned 32))
payloadCounter' (payload_counter, state) (UdpRxIn {..}) = trace (show (state, rx_first, rx_last)) $
    ((payload_counter', state'), (UdpRxOut {rx_ready = 1}, payload_counter))
    where
        state' = case (state, rx_valid, rx_last, rx_first) of
            (Idle, 1, _, 1) -> Receiving
            (Receiving, 1, 1, _) -> Idle
            _ -> state

        payload_counter' = case (state, rx_first, rx_valid) of
            (Idle, 1, 1) -> payload_counter + 1
            (Receiving, _, 1) -> payload_counter + 1
            _ -> payload_counter

testInput :: [UdpRxIn]
testInput = L.zipWith build all_firsts all_lasts
    where
        build first last = UdpRxIn {
            rx_valid = 1,
            rx_first = first,
            rx_last = last,
            rx_payload = 0xff,
            rx_last_be = 1
        }
            
        packet_lengths = [5, 6, 8, 4, 10, 12, 33, 10, 2]
        packet_breaks  = [3, 2, 4, 1,  0,  4,  1, 10, 2]

        lasts [] [] = []
        lasts (len:lengths) (break:breaks) = 
            ((L.replicate (len - 1) 0) L.++ [1] L.++ (L.replicate break 0)) L.++ (lasts lengths breaks)

        firsts []  [] = []
        firsts (len:lengths) (break:breaks) = 
            ([1] L.++ (L.replicate (len - 1) 0) L.++ (L.replicate break 0)) L.++ (firsts lengths breaks)

        all_lasts = lasts packet_lengths packet_breaks
        all_firsts = firsts packet_lengths packet_breaks

test1 = simulate @System payloadCounter testInput
test2 = simulate @System packetCounter testInput