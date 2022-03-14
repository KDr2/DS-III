-- using stdlib

{-# OPTIONS --guardedness #-}

module implicit-args where

open import IO
open import Data.String.Base

data Name : String -> Set where
  init : { s : String } -> Name s
  dot  : { s : String } -> Name s -> Name (s ++ ".")

stringify : (s : String) -> (Name s) -> String
stringify s x = s

stringify' : { s : String } -> (Name s) -> String
stringify' x = stringify _ x

stringify'' : { s : String } -> (Name s) -> String
stringify'' { s } x = s

a : Name "Hello"
a = init

f : ( s : String) -> Name s
f x = init

main : Main
main = run
   do
     s <- getLine
     putStrLn (stringify'' (dot (dot (f s))))
