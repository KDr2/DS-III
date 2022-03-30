import System.Random
import Data.Fin
import Data.String

strToNat : String -> Nat
strToNat s = case parsePositive {a=Nat} s of
  Just x => x
  Nothing => 0

myShow : (x : Nat) -> (y : Fin (S x)) -> String
myShow x y = show y


guess : (aim : Fin (S x))  -> IO ()
guess aim = do
  input <- getLine
  let try = strToNat input
  if finToNat aim == try then
    pure ()
    else do
      _ <- if finToNat aim > try then
              putStrLn "try a number bigger!"
              else putStrLn "try a number smaller!"
      guess(aim)

main : IO ()
main = do
  putStrLn "Please enter the upper bound:"
  s_max <- getLine
  putStrLn "Please enter your guess:"
  let
    m = strToNat s_max
    f = myShow {x=m}
  r <- rndFin m
  _ <- guess(r)
  putStrLn $ "You made it, the answer is " ++ (f r) ++ "!"
