-- using stdlib

{-# OPTIONS --guardedness #-}

module compact-hello where

open import IO
open import Data.Nat
open import Data.Nat.Show
open import Data.String hiding (show)

main : Main
main = run (putStrLn ("Hello, World! " ++ (show 99)))
