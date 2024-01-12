{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Comps (constJustPulser) where

import Clash.Prelude

constJustPulser :: forall delay dom a . (KnownNat delay, HiddenClockResetEnable dom, NFDataX a) => 
    a -> Signal dom (Maybe a)
constJustPulser const = mux ((==) <$> counter <*> pure 0) (pure $ Just const) (pure $ Nothing)
    where 
        counter = register (0 :: Index delay) (overflowInc <$> counter)

        overflowInc ind = if ind == (maxBound :: Index delay)
            then 0
            else ind + 1

testConstJustPulser = actual == expect
    where
        actual = sampleN @System 8 (constJustPulser @4 (1 :: Unsigned 4))
        expect = [Just 1,Just 1,Nothing,Nothing,Nothing,Just 1,Nothing,Nothing]