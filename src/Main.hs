module Main where


divisores :: Int -> [Int]
divisores a = [x | x <- [1..a], a `rem` x == 0]

esPrimo :: Int -> Bool
esPrimo a = length (divisores a) == 2

primos :: Int -> [Int]
primos a = [x | x <- [1..a], esPrimo x]

sumaPrimos :: Int -> Int
sumaPrimos a = sum $ primos a


main :: IO ()

main = do 
    putStrLn "Imprimiendo bobadas."
    putStrLn $ "Suma primos hasta 5000" ++ show (sumaPrimos 10000)