-- | Main entry point to the application.
module Main2 where
import           Prelude
import Personas



data FailableDouble = Failure
    | Ok Double
    deriving Show

data Sex = Male | Female
data Telephone = Telephone String
data StreetAddress = Address String
data Email = Email String
data Password = Password String
data UserInfo = UserInfo Email Password StreetAddress Telephone Sex

email :: UserInfo -> String
email (UserInfo (Email s) _ _ _ _) = s


divideBy :: Double -> Double -> FailableDouble
divideBy _ 0 = Failure
divideBy a b = Ok  (a/b)

lastOfList :: [a] -> Maybe a
lastOfList [] = Nothing
lastOfList (a:[]) = Just a
lastOfList (_:t) = lastOfList t

previousOfLastOfList :: [a] -> Maybe a
previousOfLastOfList [] = Nothing
previousOfLastOfList (_:[]) = Nothing
previousOfLastOfList (a:_:[]) = Just a
previousOfLastOfList (_:xs) = previousOfLastOfList xs

elementAt :: [a] -> Integer -> Maybe a
elementAt [] _ = Nothing
elementAt (a:_) 0 = Just a
elementAt (_:xs) b = elementAt xs (b-1)

myLength :: [a] -> Integer
myLength []  = 0
myLength (_:xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (a:xs) = (myReverse xs) ++ [a]

isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome (_:[]) = True
isPalindrome (a:b:[]) =  a == b
isPalindrome l = (head l) == (head (myReverse l)) && isPalindrome (tail (myReverse (tail l)))

data NestedList a = Elem a | List [NestedList a] deriving Show
flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List []) = []
flatten (List [Elem a]) = [a]
flatten (List (a:xs)) = flatten a ++ flatten (List xs)

compress :: Eq a => [a] -> [a]
compress [] = []
compress [a] = [a]
compress (a:b:xs) = if a == b
    then compress ([a] ++ compress xs)
    else [a] ++ compress(b:xs)

--pack :: Eq a => [a] -> [[a]]

cosas :: [Integer]
cosas = [1, 2, 3, 4, 23]

mem :: Personas.PersonaMemory
mem = crearPersona initPersonaMemory "Victor" "095253555" "8 de oct 2323" 33
-- | The main entry point.
main :: IO ()
main = do
    putStrLn "Welcome to FP Haskell Center!"
    putStrLn "Have a good day!"
    print cosas
    print $ lastOfList cosas
    print (previousOfLastOfList cosas)
    print (elementAt cosas 0)
    print (elementAt cosas 1)
    print (elementAt cosas 2)
    print (elementAt cosas 3)
    print (elementAt cosas 4)
    print (elementAt cosas 5)
    print (divideBy 3 2)
    print (myLength cosas)
    print (myReverse cosas)
    print (isPalindrome "oakao")
    print (isPalindrome "victorrotciv")
    print (isPalindrome "victorv")
    print (flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]))
    print (compress "11111111111123333333456677778778")
    
    print $ getPersona ((mem1)) 1
mem1 = crearPersona mem "Roberto" "333" "otra dir" 23


