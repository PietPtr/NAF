{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
module Transactions (Transaction, filterTransactions, didTransact) where

import Data.Proxy
import Clash.Prelude hiding (last, (++))
import qualified Data.List as List
import Data.Maybe

class Transaction inp out where
    -- Given the interface, did a transaction happen at this cycle?
    didTransact :: inp -> out -> Bit

-- Output maybes to maintain 1 cycle == 1 list entry
filterTransactions :: (Transaction inp out) => [(inp, out)] -> [Maybe (inp, out)]
filterTransactions = List.map (\(inp, out) -> if didTransact inp out == 1 then Just (inp, out) else Nothing ) 

-- TODO: typeclass for things with first/last/payload, and a function to extract full payload