open import Relation.Nullary
open import Data.Nat
open import Relation.Binary.PropositionalEquality using (_≡_)

p0 : ¬ 0 ≡ 1 -- 0 ≡ 1 → ⊥
p0 ()


p1 : ∀ {n : ℕ } → ¬ n ≡ suc n
p1 ()
