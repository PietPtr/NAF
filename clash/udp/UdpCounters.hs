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

        udp_tx_out = VecAsUdp.sendVec @32 udp_tx_in (f <$> udp_rx_in <*> packet_count <*> payload_count)
        
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
payloadCounter' (payload_counter, state) (UdpRxIn {..}) =
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

test1 = actual == expect
    where
        actual = L.take 116 $ simulate @System payloadCounter testInput
        expect = [(UdpRxOut {rx_ready = 1},0),(UdpRxOut {rx_ready = 1},1),(UdpRxOut {rx_ready = 1},2),(UdpRxOut {rx_ready = 1},3),(UdpRxOut {rx_ready = 1},4),(UdpRxOut {rx_ready = 1},5),(UdpRxOut {rx_ready = 1},5),(UdpRxOut {rx_ready = 1},5),(UdpRxOut {rx_ready = 1},5),(UdpRxOut {rx_ready = 1},6),(UdpRxOut {rx_ready = 1},7),(UdpRxOut {rx_ready = 1},8),(UdpRxOut {rx_ready = 1},9),(UdpRxOut {rx_ready = 1},10),(UdpRxOut {rx_ready = 1},11),(UdpRxOut {rx_ready = 1},11),(UdpRxOut {rx_ready = 1},11),(UdpRxOut {rx_ready = 1},12),(UdpRxOut {rx_ready = 1},13),(UdpRxOut {rx_ready = 1},14),(UdpRxOut {rx_ready = 1},15),(UdpRxOut {rx_ready = 1},16),(UdpRxOut {rx_ready = 1},17),(UdpRxOut {rx_ready = 1},18),(UdpRxOut {rx_ready = 1},19),(UdpRxOut {rx_ready = 1},19),(UdpRxOut {rx_ready = 1},19),(UdpRxOut {rx_ready = 1},19),(UdpRxOut {rx_ready = 1},19),(UdpRxOut {rx_ready = 1},20),(UdpRxOut {rx_ready = 1},21),(UdpRxOut {rx_ready = 1},22),(UdpRxOut {rx_ready = 1},23),(UdpRxOut {rx_ready = 1},23),(UdpRxOut {rx_ready = 1},24),(UdpRxOut {rx_ready = 1},25),(UdpRxOut {rx_ready = 1},26),(UdpRxOut {rx_ready = 1},27),(UdpRxOut {rx_ready = 1},28),(UdpRxOut {rx_ready = 1},29),(UdpRxOut {rx_ready = 1},30),(UdpRxOut {rx_ready = 1},31),(UdpRxOut {rx_ready = 1},32),(UdpRxOut {rx_ready = 1},33),(UdpRxOut {rx_ready = 1},34),(UdpRxOut {rx_ready = 1},35),(UdpRxOut {rx_ready = 1},36),(UdpRxOut {rx_ready = 1},37),(UdpRxOut {rx_ready = 1},38),(UdpRxOut {rx_ready = 1},39),(UdpRxOut {rx_ready = 1},40),(UdpRxOut {rx_ready = 1},41),(UdpRxOut {rx_ready = 1},42),(UdpRxOut {rx_ready = 1},43),(UdpRxOut {rx_ready = 1},44),(UdpRxOut {rx_ready = 1},45),(UdpRxOut {rx_ready = 1},45),(UdpRxOut {rx_ready = 1},45),(UdpRxOut {rx_ready = 1},45),(UdpRxOut {rx_ready = 1},45),(UdpRxOut {rx_ready = 1},46),(UdpRxOut {rx_ready = 1},47),(UdpRxOut {rx_ready = 1},48),(UdpRxOut {rx_ready = 1},49),(UdpRxOut {rx_ready = 1},50),(UdpRxOut {rx_ready = 1},51),(UdpRxOut {rx_ready = 1},52),(UdpRxOut {rx_ready = 1},53),(UdpRxOut {rx_ready = 1},54),(UdpRxOut {rx_ready = 1},55),(UdpRxOut {rx_ready = 1},56),(UdpRxOut {rx_ready = 1},57),(UdpRxOut {rx_ready = 1},58),(UdpRxOut {rx_ready = 1},59),(UdpRxOut {rx_ready = 1},60),(UdpRxOut {rx_ready = 1},61),(UdpRxOut {rx_ready = 1},62),(UdpRxOut {rx_ready = 1},63),(UdpRxOut {rx_ready = 1},64),(UdpRxOut {rx_ready = 1},65),(UdpRxOut {rx_ready = 1},66),(UdpRxOut {rx_ready = 1},67),(UdpRxOut {rx_ready = 1},68),(UdpRxOut {rx_ready = 1},69),(UdpRxOut {rx_ready = 1},70),(UdpRxOut {rx_ready = 1},71),(UdpRxOut {rx_ready = 1},72),(UdpRxOut {rx_ready = 1},73),(UdpRxOut {rx_ready = 1},74),(UdpRxOut {rx_ready = 1},75),(UdpRxOut {rx_ready = 1},76),(UdpRxOut {rx_ready = 1},77),(UdpRxOut {rx_ready = 1},78),(UdpRxOut {rx_ready = 1},78),(UdpRxOut {rx_ready = 1},79),(UdpRxOut {rx_ready = 1},80),(UdpRxOut {rx_ready = 1},81),(UdpRxOut {rx_ready = 1},82),(UdpRxOut {rx_ready = 1},83),(UdpRxOut {rx_ready = 1},84),(UdpRxOut {rx_ready = 1},85),(UdpRxOut {rx_ready = 1},86),(UdpRxOut {rx_ready = 1},87),(UdpRxOut {rx_ready = 1},88),(UdpRxOut {rx_ready = 1},88),(UdpRxOut {rx_ready = 1},88),(UdpRxOut {rx_ready = 1},88),(UdpRxOut {rx_ready = 1},88),(UdpRxOut {rx_ready = 1},88),(UdpRxOut {rx_ready = 1},88),(UdpRxOut {rx_ready = 1},88),(UdpRxOut {rx_ready = 1},88),(UdpRxOut {rx_ready = 1},88),(UdpRxOut {rx_ready = 1},88),(UdpRxOut {rx_ready = 1},89),(UdpRxOut {rx_ready = 1},90)]

test2 = actual == expect
    where
        actual = L.take 116 $ simulate @System packetCounter testInput
        expect = [(UdpRxOut {rx_ready = 1},0),(UdpRxOut {rx_ready = 1},1),(UdpRxOut {rx_ready = 1},1),(UdpRxOut {rx_ready = 1},1),(UdpRxOut {rx_ready = 1},1),(UdpRxOut {rx_ready = 1},1),(UdpRxOut {rx_ready = 1},1),(UdpRxOut {rx_ready = 1},1),(UdpRxOut {rx_ready = 1},1),(UdpRxOut {rx_ready = 1},2),(UdpRxOut {rx_ready = 1},2),(UdpRxOut {rx_ready = 1},2),(UdpRxOut {rx_ready = 1},2),(UdpRxOut {rx_ready = 1},2),(UdpRxOut {rx_ready = 1},2),(UdpRxOut {rx_ready = 1},2),(UdpRxOut {rx_ready = 1},2),(UdpRxOut {rx_ready = 1},3),(UdpRxOut {rx_ready = 1},3),(UdpRxOut {rx_ready = 1},3),(UdpRxOut {rx_ready = 1},3),(UdpRxOut {rx_ready = 1},3),(UdpRxOut {rx_ready = 1},3),(UdpRxOut {rx_ready = 1},3),(UdpRxOut {rx_ready = 1},3),(UdpRxOut {rx_ready = 1},3),(UdpRxOut {rx_ready = 1},3),(UdpRxOut {rx_ready = 1},3),(UdpRxOut {rx_ready = 1},3),(UdpRxOut {rx_ready = 1},4),(UdpRxOut {rx_ready = 1},4),(UdpRxOut {rx_ready = 1},4),(UdpRxOut {rx_ready = 1},4),(UdpRxOut {rx_ready = 1},4),(UdpRxOut {rx_ready = 1},5),(UdpRxOut {rx_ready = 1},5),(UdpRxOut {rx_ready = 1},5),(UdpRxOut {rx_ready = 1},5),(UdpRxOut {rx_ready = 1},5),(UdpRxOut {rx_ready = 1},5),(UdpRxOut {rx_ready = 1},5),(UdpRxOut {rx_ready = 1},5),(UdpRxOut {rx_ready = 1},5),(UdpRxOut {rx_ready = 1},5),(UdpRxOut {rx_ready = 1},6),(UdpRxOut {rx_ready = 1},6),(UdpRxOut {rx_ready = 1},6),(UdpRxOut {rx_ready = 1},6),(UdpRxOut {rx_ready = 1},6),(UdpRxOut {rx_ready = 1},6),(UdpRxOut {rx_ready = 1},6),(UdpRxOut {rx_ready = 1},6),(UdpRxOut {rx_ready = 1},6),(UdpRxOut {rx_ready = 1},6),(UdpRxOut {rx_ready = 1},6),(UdpRxOut {rx_ready = 1},6),(UdpRxOut {rx_ready = 1},6),(UdpRxOut {rx_ready = 1},6),(UdpRxOut {rx_ready = 1},6),(UdpRxOut {rx_ready = 1},6),(UdpRxOut {rx_ready = 1},7),(UdpRxOut {rx_ready = 1},7),(UdpRxOut {rx_ready = 1},7),(UdpRxOut {rx_ready = 1},7),(UdpRxOut {rx_ready = 1},7),(UdpRxOut {rx_ready = 1},7),(UdpRxOut {rx_ready = 1},7),(UdpRxOut {rx_ready = 1},7),(UdpRxOut {rx_ready = 1},7),(UdpRxOut {rx_ready = 1},7),(UdpRxOut {rx_ready = 1},7),(UdpRxOut {rx_ready = 1},7),(UdpRxOut {rx_ready = 1},7),(UdpRxOut {rx_ready = 1},7),(UdpRxOut {rx_ready = 1},7),(UdpRxOut {rx_ready = 1},7),(UdpRxOut {rx_ready = 1},7),(UdpRxOut {rx_ready = 1},7),(UdpRxOut {rx_ready = 1},7),(UdpRxOut {rx_ready = 1},7),(UdpRxOut {rx_ready = 1},7),(UdpRxOut {rx_ready = 1},7),(UdpRxOut {rx_ready = 1},7),(UdpRxOut {rx_ready = 1},7),(UdpRxOut {rx_ready = 1},7),(UdpRxOut {rx_ready = 1},7),(UdpRxOut {rx_ready = 1},7),(UdpRxOut {rx_ready = 1},7),(UdpRxOut {rx_ready = 1},7),(UdpRxOut {rx_ready = 1},7),(UdpRxOut {rx_ready = 1},7),(UdpRxOut {rx_ready = 1},7),(UdpRxOut {rx_ready = 1},7),(UdpRxOut {rx_ready = 1},7),(UdpRxOut {rx_ready = 1},8),(UdpRxOut {rx_ready = 1},8),(UdpRxOut {rx_ready = 1},8),(UdpRxOut {rx_ready = 1},8),(UdpRxOut {rx_ready = 1},8),(UdpRxOut {rx_ready = 1},8),(UdpRxOut {rx_ready = 1},8),(UdpRxOut {rx_ready = 1},8),(UdpRxOut {rx_ready = 1},8),(UdpRxOut {rx_ready = 1},8),(UdpRxOut {rx_ready = 1},8),(UdpRxOut {rx_ready = 1},8),(UdpRxOut {rx_ready = 1},8),(UdpRxOut {rx_ready = 1},8),(UdpRxOut {rx_ready = 1},8),(UdpRxOut {rx_ready = 1},8),(UdpRxOut {rx_ready = 1},8),(UdpRxOut {rx_ready = 1},8),(UdpRxOut {rx_ready = 1},8),(UdpRxOut {rx_ready = 1},8),(UdpRxOut {rx_ready = 1},9),(UdpRxOut {rx_ready = 1},9)]