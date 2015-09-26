module Dinero2 where

data TipoMoneda = Dolar | PesoUruguayo deriving Show

data Dinero = Dinero {
    monto :: Double,
    moneda :: TipoMoneda
} deriving Show

