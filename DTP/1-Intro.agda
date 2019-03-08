module 1-Intro where

  {- Courtesy of Conor McBride -}

-- Not all things are the same. This
-- gives us something to care about.

-- Haskell:
--   data Bool = False | True

-- DTP 裡 value & type 是同一層, data type 和 constructors 都可以是小寫開頭
data Bool : Set where
  false : Bool
  true : Bool

-- Programs often produce different output,
-- given different input.

not : Bool → Bool
not false = true
not true  = false

  -- The type could also be written
  --   not : (b : Bool) → Bool

data ℕ : Set where  -- ℕ: \bn
  zero : ℕ
  suc  : ℕ → ℕ

{-# BUILTIN NATURAL ℕ #-}

  -- The derivative above allows us to use built-in
  -- abbreviations, e.g. 0 for zero, 1 for suc zero,
  -- 2 for suc (suc zero).

-- Types could be parameterised by types.
 --  data List A = [] | A ∷ List A

data List (A : Set) : Set where
  [] : List A
  _∷_ : A → List A → List A   -- 有底線表示可以放參數的地方

  -- A is in the scope for the constructors.

infixr 5 _∷_  -- declare the precdence and associativity of _∷_.

ex1 : List ℕ
ex1  = suc zero ∷ suc (suc zero) ∷ suc(suc(suc zero )) ∷ [] -- the list [1,2,3].

-- Type argument must be given,

id₁ : (A : Set) → A → A
id₁ A x = x

   -- e.g. id₁ Bool true

-- unless declared implicit.

id : {A : Set} → A → A  -- implicit type: agda 會去猜
id x = x

   -- e.g. id true
   --      id {Bool} true

-- It's often important to know whether a
-- list is [] or not.

null : ∀ {A} → List A → Bool  -- {A} = {A : Set}
null [] = true
null (x ∷ xs) = false

-- Some more exercises/reviews on functions on lists.

infixr 5 _++_

length : ∀ {A} → List A → ℕ
length [] = 0
length (x ∷ xs) = suc (length xs)

_++_ : ∀ {A} → List A → List A → List A
[] ++ ys = ys
(x ∷ xs) ++ ys = x ∷ (xs ++ ys)

map : ∀ {A B} → (A → B) → List A → List B
map f [] = []
map f (x ∷ xs) = f x ∷ map f xs

-- _-_ : ∀{A} →

take : ∀ {A} → ℕ → List A → List A   -- ∀: \all
take zero xs = xs
take (suc n) [] = []
take (suc n) (x ∷ xs) = x ∷ take n xs

filter : ∀ {A} → (A → Bool) → List A → List A
filter p [] = []
filter p (x ∷ xs) with p x
filter p (x ∷ xs) | false = filter p xs
filter p (x ∷ xs) | true = x ∷ filter p xs
--             ...| false = filter p xs
--             ...| true = x ∷ filter p xs

   -- ∀ {A} is a shorter syntax for
   --  {A : τ} when τ can be inferred.
   -- ∀ A is a shorter synatx for
   --  (A : τ) when τ can be inferred.

{- * The following function will be rejected by Agda.
     Why?   Incomplete pattern

head : ∀ {A} → List A → A
head (x ∷ xs) = x
-}

{- * xs ‼ n intendes to extract the nth element of xs.
     The intention is, for example,
         (1 ∷ 2 ∷ 3 ∷ []) ‼ 1  =  2
    (There is a corresponding definition in Haskell called (!!)).
    Try completing the definition.

_‼_ : ∀ {A} → List A → ℕ → A
(x ∷ xs) ‼ zero = x
(x ∷ xs) ‼ suc n = xs ‼ n


    There will be one case where we don't know what to do, however. -}

-- postulate error : {X : Set} → X

-- Dependent types to the rescue:
--   boolean preconditions.

data ⊤ : Set where    -- data Top = TT
   tt : ⊤

data ⊥ : Set where    -- an np value "type"  做不出屬於這個type的值

   -- It's our first function from a value
   -- to a type

IsTrue : Bool → Set
IsTrue false = ⊥
IsTrue true  = ⊤

headOk : ∀ {A} → (xs : List A) →
            (IsTrue (not (null xs))) → A
headOk [] ()          -- 保證 list 不會是空的，[]不會被call
headOk (x ∷ xs) p = x

{- * Use headOk to extract the first component of ex1 -}

headex1 : ℕ
headex1 = headOk ex1 tt

{- * Can you apply headOk to []? How, or why not?   ANs: not. -}
   -- ..... headOk [] (...) ...

last : ∀ {A} → (xs : List A) → IsTrue (not (null xs)) → A
last [] ()
last (x ∷ []) p = x
last (x ∷ x₁ ∷ xs) p = last (x₁ ∷ xs) p


-- a more complex example

_∨_ : Bool → Bool → Bool
true  ∨ q = true
false ∨ b = b

_∧_ : Bool → Bool → Bool
false ∧ c = false
true ∧ false = false
true ∧ true = true

somewhere : ∀ {A} → (A → Bool) → List A → Bool
somewhere p [] = false
somewhere p (x ∷ xs) = p x ∨ somewhere p xs

find1st : ∀{A} → (p : A → Bool) → (xs : List A) →
           IsTrue (somewhere p xs) → A
find1st p [] ()
find1st p (x ∷ xs) q with p x
...| false = find1st p xs q
...| true = x
-- Equality for ℕ

_==_ : ℕ → ℕ → Bool
zero == zero = true
zero == suc n = false
suc m == zero = false
suc m == suc n = m == n

-- Less-than-or-equal-to for ℕ

_≤_ : ℕ → ℕ → Bool
zero ≤ n = true
suc m ≤ zero = false
suc m ≤ suc n = m ≤ n

_<_ : ℕ → ℕ → Bool
zero < zero = false
zero < suc n = true
suc m < zero = false
suc m < suc n = m < n


 {- This is a safe version of _‼_, in which we demand that, whenever
    we call index xs n, we must have shown that n < length xs -}

index : ∀ {A} → (xs : List A) → (n : ℕ) → IsTrue (n < length xs) → A
index [] zero ()
index [] (suc n) ()
index (x ∷ xs) zero tt = x
index (x ∷ xs) (suc n) p = index xs n p
