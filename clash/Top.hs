module Top where 

import Clash.Prelude

system :: HiddenClockResetEnable dom =>
    Signal dom (Maybe (Unsigned 32)) -> Signal dom (Maybe (Unsigned 32))
system inp = fmap (fmap (+1)) inp

{-# ANN topEntity
  (Synthesize
    { t_name   = "main"
    , t_inputs = [ PortName "clock"
                 , PortName "reset"
                 , PortName "enable"
                 , PortName "data_in"]
    , t_output = PortName "data_out"
    }) #-}
topEntity ::
     Clock System
     -> Reset System
     -> Enable System
     -> Signal System (Maybe (Unsigned 32))
     -> Signal System (Maybe (Unsigned 32))
topEntity = exposeClockResetEnable system