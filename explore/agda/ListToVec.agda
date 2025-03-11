{-# OPTIONS --guardedness #-}
module ListToVec where

open import Data.List using (List; []; _∷_; length)
open import Data.Nat using (ℕ)
open import Data.Vec using (Vec; []; _∷_)
open import Data.String using (String; _++_)
open import Function using (_$_)
open import IO
open import System.Environment

fromList : (xs : List String) → Vec String (length xs)
fromList [] = []
fromList (x ∷ xs) = x ∷ fromList xs

vecToString : ∀ {n} → Vec String n → String
vecToString [] = "[]"
vecToString (x ∷ xs) = x ++ " ∷ " ++ vecToString xs

main : Main
main = run $ do
  args ← getArgs
  putStrLn (vecToString (fromList args))