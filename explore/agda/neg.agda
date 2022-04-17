open import Data.Nat
open import Relation.Nullary
open import Relation.Binary.PropositionalEquality using (_≡_; cong)

p0 : ¬ 0 ≡ 1 -- 0 ≡ 1 → ⊥
p0 ()

p1 : ¬ 1 ≡ 2
p1 e = p0 (cong (\ x -> x ∸ 1) e)

--------

q0 : ∀ { n : ℕ } → ¬ 0 ≡ suc n
q0 ()

red0 : ∀ { m n : ℕ } → m ≡ m + (suc n) → 0 ≡ suc n
red0 {0} e = e
red0 {suc m} e = red0 {m} (cong (\ x -> x ∸ 1) e)

q1 : ∀ { m n : ℕ } → ¬ m ≡ m + (suc n)
q1 e = q0 (red0 e)

------

data _=/=_ : ( n : ℕ ) → ( m : ℕ ) -> Set where
  base : ∀ { x : ℕ } -> zero =/= suc x
  nsuc : { n m d : ℕ } → n =/= m → n + d =/= m + d

infix 1 _=/=_

d1 : 3 =/= 5
d1 = nsuc { d = 3 } (base { x = 1})
