{-# OPTIONS --guardedness #-}

module ffi-demo where

open import IO
open import Agda.Builtin.String
open import Agda.Builtin.List using([])
open import Data.Char.Base using (fromℕ)
open import Data.Integer.Base using (ℤ)
open import Data.Integer.Show renaming (show to showℤ)
open import Data.String.Base

postulate
  AgdaShow : Set -> Set
  Str : Set
  agdaShow : {A : Set} -> AgdaShow A -> Str

{-# FOREIGN GHC data Show' a = Show a => ShowI #-}
{-# COMPILE GHC AgdaShow = type Show' #-}
{-# COMPILE GHC Str = type [Char] #-}
-- {-# COMPILE GHC agdaShow = show #-}
-- {-# COMPILE GHC agdaShow = \ _ ShowI -> (show) #-}

-- a : Str
-- a = agdaShow 1

-- parse String to Integer
postulate strToInt : String -> ℤ
{-# FOREIGN GHC import Data.Text #-}
{-# FOREIGN GHC hsStrToInt :: Data.Text.Text -> Integer; hsStrToInt x = read(unpack x) #-}
{-# COMPILE GHC strToInt = hsStrToInt #-}

main : Main
main = run
  do
    s <- getLine
    let a = strToInt s
    putStrLn (showℤ a)
    putStrLn ("FFI Demo")
