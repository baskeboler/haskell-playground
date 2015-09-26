{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Dinero where
import Data.Typeable
import Data.HashMap.Strict as HM


class Typeable m => Dinero m where
    dinero :: (Dinero m) => Double -> m
    cantidad :: (Dinero m) => m -> Double
    sumar :: (Dinero m) => m -> m -> m
    sumar a b = dinero $ (cantidad a) + (cantidad b)


newtype Dolar = Dolar Double
    deriving (Show, Eq, Typeable)

instance Dinero Dolar where
    dinero = Dolar
    cantidad (Dolar a) = a

newtype PesoUruguayo = PesoUruguayo Double
    deriving (Show, Eq, Typeable)

instance Dinero PesoUruguayo where
    dinero = PesoUruguayo
    cantidad (PesoUruguayo a) = a

newtype TasasDeCAmbio = Cambio (HashMap TypeRep Double)
    deriving Show

buscarCambio :: Typeable a => TasasDeCAmbio -> a -> Maybe Double
buscarCambio (Cambio m) a = HM.lookup (typeOf a) m

realizarCambio :: forall a b. (Dinero a, Dinero b) => TasasDeCAmbio -> a -> Maybe b
realizarCambio tasas a = do
  aRate <- buscarCambio tasas a
  bRate <- buscarCambio tasas (undefined :: b)

  return $ dinero (bRate * (cantidad a / aRate))

tasasDeEjemplo :: TasasDeCAmbio
tasasDeEjemplo = Cambio $ HM.fromList [ (typeOf (Dolar 0), 1), (typeOf (PesoUruguayo 0), 29.3333)    ]

dolaresEjemplo :: Dolar
dolaresEjemplo = Dolar 7

pesosEjemplo :: PesoUruguayo
pesosEjemplo = PesoUruguayo 133

cambioAPesos :: Maybe PesoUruguayo
cambioAPesos = realizarCambio tasasDeEjemplo dolaresEjemplo :: Maybe PesoUruguayo

cambioADolares :: Maybe Dolar
cambioADolares = realizarCambio tasasDeEjemplo pesosEjemplo :: Maybe Dolar

main :: IO ()
main = do
    print cambioAPesos
    print cambioADolares
