{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where
import Control.Monad.Reader
import Control.Monad.Identity

newtype MyEnvironment a = MyEnvironment { runMyEnvironment :: Reader Int a } deriving (Monad, MonadReader Int)

divisores :: Int -> [Int]
divisores a = [x | x <- [1..a], a `rem` x == 0]

esPrimo :: Int -> Bool
esPrimo a = length (divisores a) == 2

primos :: Int -> [Int]
primos a = [x | x <- [1..a], esPrimo x]

sumaPrimos :: Int -> Int
sumaPrimos a = sum $ primos a


r :: MyEnvironment Int
r  = do 
    r <- MyEnvironment $ ask
    return r

main :: IO ()

main = do 
    putStrLn "Imprimiendo bobadas."
    putStrLn $ "Suma primos hasta 5000" ++ (show $ sumaPrimos 5000)