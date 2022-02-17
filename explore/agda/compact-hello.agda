-- using stdlib

{-# OPTIONS --guardedness #-}

module compact-hello where

open import IO

main : Main
main = run (putStrLn "Hello, World!")
