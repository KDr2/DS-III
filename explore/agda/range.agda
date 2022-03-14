{-# OPTIONS --guardedness #-}

module range where

open import IO
open import Data.Nat using (ℕ; zero; suc)
open import Data.Nat.Show
open import Data.String.Base hiding (_≤_; show)

data _≤_ :  ℕ → ℕ → Set where
  instance i : ∀ {n : ℕ} → zero ≤ n
  instance s : ∀ {m n : ℕ} → ⦃ m ≤ n ⦄ → suc m ≤ suc n

data NatInRange (l : ℕ) (u : ℕ) : Set where
  nat-in-r : (n : ℕ) -> ⦃ l ≤ n ⦄ -> ⦃ n ≤ u ⦄ -> NatInRange l u

n : NatInRange 10 20
n = nat-in-r 12

str : ∀ {m n : ℕ} -> (NatInRange m n) -> String
str {m} {n} (nat-in-r x) = "[" ++ show m ++ " <- " ++ show x ++ " -> " ++ show n ++ "]"

main : Main
main = run
  do
    putStrLn "Range"
    putStrLn (str n)
    putStrLn (str {2} {10} (nat-in-r 4))
    putStrLn "End"
