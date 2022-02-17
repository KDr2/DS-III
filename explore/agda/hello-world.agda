module hello-world where

open import Agda.Builtin.IO using (IO)
open import Agda.Builtin.Unit renaming (⊤ to Top)
open import Agda.Builtin.String using (String)

postulate putStrLn : String → IO Top
{-# FOREIGN GHC import qualified Data.Text as T #-}
{-# COMPILE GHC putStrLn = putStrLn . T.unpack #-}

main : IO Top
main = putStrLn "Hello world!"
