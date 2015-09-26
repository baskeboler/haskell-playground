{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where
import Control.Monad.Reader
import Control.Monad.Identity

newtype MyEnvironment a = MyEnvironment { runMyEnvironment :: Reader Int a } deriving (Monad, MonadReader Int)

r :: MyEnvironment Int
r  = do 
    r <- MyEnvironment $ ask
    return r

main :: IO ()

main = putStrLn . show $ (runMyEnvironment r) `runReader` 4