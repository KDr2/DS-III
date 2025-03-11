{-# OPTIONS --guardedness #-}

module DynamicVectorWithMain where

open import IO
open import Data.String using (String; _++_)
open import Data.Nat using (ℕ; zero; suc; _+_)
open import Data.Nat.Show using (show)
open import Data.Vec using (Vec; []; _∷_)
open import Data.Product using (Σ; _,_)
open import Data.Bool using (Bool; true; false)

DynamicVec : Set → Set
DynamicVec A = Σ ℕ (λ n → Vec A n)

createVec : Bool → DynamicVec ℕ
createVec true  = 1 , 1 ∷ []
createVec false = 2 , 1 ∷ 2 ∷ []

sumVec : DynamicVec ℕ → ℕ
sumVec (zero  , [])     = 0
sumVec (suc n , x ∷ xs) = x + sumVec (n , xs)

showResult : Bool → String
showResult b = "Result: " ++ show (sumVec (createVec b))

main : Main
main = run do
  putStrLn (showResult true)
  putStrLn (showResult false)