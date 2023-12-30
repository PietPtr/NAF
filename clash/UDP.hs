{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
module UDP where

import Clash.Prelude hiding (last, (++))
import qualified Data.List as List
import Data.List ((++))
import Data.Maybe

class Transaction inp out where
    -- Given the interface, did a transaction happen at this cycle?
    didTransact :: inp -> out -> Bit

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

constantUdpStreamer :: HiddenClockResetEnable dom =>
    Signal dom UdpTxIn -> Signal dom UdpTxOut
constantUdpStreamer = mealy constantUdpStreamer' def

data SendState = Idle | Sending (Unsigned 8)
    deriving (Generic, NFDataX, Show, Eq)

instance Default SendState where
    def = Idle

data ConstantUdpStreamerState = ConstantUdpStreamerState {
    cycle_counter :: Unsigned 12,
    send_state :: SendState
} deriving (Generic, NFDataX, Default, Show)

constantUdpStreamer' :: ConstantUdpStreamerState -> UdpTxIn -> (ConstantUdpStreamerState, UdpTxOut)
constantUdpStreamer' state (UdpTxIn { tx_ready = ready }) = (state', out)
    where
        ConstantUdpStreamerState{..} = state

        out = UdpTxOut {
            tx_valid = valid,
            tx_first = first,
            tx_last = last,
            tx_payload = payload
        }

        packet_size = 32

        send_state' = case send_state of
            Idle -> if cycle_counter == 0 then Sending packet_size else Idle
            Sending 0 -> Idle
            Sending n -> if ready == 1 then Sending (n - 1) else Sending n

        first = bitCoerce $ send_state == Sending packet_size
        last = bitCoerce $ send_state == Sending 0
        valid = bitCoerce $ send_state /= Idle

        payload = case send_state of
            Sending n -> pack n
            Idle -> 0xff
        
        state' = ConstantUdpStreamerState {
            cycle_counter = cycle_counter + 1,
            send_state = send_state'
        }


-- TODO: make sim lib with all these helper functions and some of the types above
testInput :: [UdpTxIn]
testInput = [UdpTxIn r | r <- cycle [0, 0, 0, 0, 0, 1, 0, 0, 1, 1]]  -- Alternating ready signal

simulateUdpStreamer :: Int -> [(UdpTxIn, UdpTxOut)]
simulateUdpStreamer n = List.zip testInput output
    where output = List.take n $ simulate @System constantUdpStreamer testInput

filterTransactions :: (Transaction inp out) => [(inp, out)] -> [Maybe (inp, out)]
filterTransactions = List.map (\(inp, out) -> if didTransact inp out == 1 then Just (inp, out) else Nothing ) 

prettyPerCycleFiltered :: (a -> Bool) -> (a -> String) -> [a] -> [String]
prettyPerCycleFiltered filter formatter waves = List.map (\(cycle, wave) -> show cycle ++ ": " ++ formatter wave) with_cycles_filtered
    where
        with_cycles = List.zip [0..] waves
        with_cycles_filtered = List.filter (\(cycle, wave) -> filter wave) with_cycles

prettyPerCycle :: (a -> String) -> [a] -> [String]
prettyPerCycle = prettyPerCycleFiltered (const True)

maybePrintPerCycle :: (Show a) => [Maybe a] -> IO ()
maybePrintPerCycle sim_output = mapM_ putStrLn (prettyPerCycleFiltered isJust show sim_output)

printPerCycle :: (Show a) => [a] -> IO ()
printPerCycle sim_output = mapM_ putStrLn (prettyPerCycle show sim_output)

wavesPerCycle :: (Show a, Show b) => [(a, b)] -> IO ()
wavesPerCycle sim_output = mapM_ putStrLn (prettyPerCycle (\(inp, out) -> show inp ++ " -> " ++ show out) sim_output)

test1 = maybePrintPerCycle $ filterTransactions $ simulateUdpStreamer 300