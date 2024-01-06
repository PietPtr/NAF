{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Comps (constJustPulser) where

import Clash.Prelude

constJustPulser :: forall delay dom a . (KnownNat delay, HiddenClockResetEnable dom) => 
    a -> Signal dom (Maybe a)
constJustPulser const = mux ((==) <$> counter <*> pure 0) (pure Nothing) (pure $ Just const)
    where counter = register (0 :: Index delay) (counter + 1)

testConstJustPulser = simulate @System (constJustPulser @4 1)