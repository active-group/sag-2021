{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module IntroMike where

import Prelude hiding (Functor, Monad, Monoid, Semigroup)

x :: Integer
x = 12

f :: Integer -> Integer
f x = x + 5

-- Animal on the Texas highway is one of the following:
-- - armadillo - OR -
-- - parrot

-- Armadillo has the following properties:
-- - dead or alive - AND -
-- - weight 

-- Parrot has the following properties:
-- - sentence
-- - weight

data Liveness = Dead | Alive
  deriving Show -- makes Liveness printable
-- data: new datatype, | = or

type Weight = Integer 
-- type synonym

-- "state of the animal at a certain time"
data Animal = 
    Dillo Liveness Weight
  | Parrot String Weight
  deriving Show

-- live armadillo, 10kg
dillo1 :: Animal
dillo1 = Dillo Alive 10

-- dead armadillo, 12kg
dillo2 :: Animal
dillo2 = Dillo Dead 12

-- welcome parrot, 1kg
parrot1 :: Animal
parrot1 = Parrot "Hello!" 1

parrot2 :: Animal
parrot2 = Parrot "Bye!" 2


{-
class Dillo {
    Liveness liveness;
    Weight weight;

    void runOver() { this.liveness = DEAD; }
}

-}

-- run over an animal
runOverAnimal :: Animal -> Animal
-- pattern matching
runOverAnimal (Dillo l w) = Dillo Dead w
runOverAnimal (Parrot sentence weight) = Parrot "" weight

-- Haskell knows only unary functions
-- "curried functions"
-- Haskell Curry
-- Moses Schönfinkel
-- other such functional languages: F#, OCaml, Elm
-- not such functional languages: Racket, Scala, Erlang

-- feed animal

feedAnimal :: Weight -> (Animal -> Animal)
feedAnimal amount (Dillo Alive weight) = Dillo Alive (weight + amount)
feedAnimal amount (Dillo Dead weight) = Dillo Dead weight
feedAnimal amount (Parrot sentence weight) = Parrot sentence (weight + amount)

feedAnimal' :: (Weight, Animal) -> Animal
feedAnimal' (amount, Dillo Alive weight) = Dillo Alive (weight + amount)
feedAnimal' (amount, Dillo Dead weight) = Dillo Dead weight
feedAnimal' (amount, Parrot sentence weight) = Parrot sentence (weight + amount)

-- mycurry :: ((Weight, Animal) -> Animal) -> (Weight -> Animal -> Animal)
mycurry :: ((a, b) -> c) -> (a -> b -> c)
-- mycurry f = \ weight -> \ animal -> f (weight, animal)
mycurry f = \ a -> \ b -> f (a, b)

-- List is one of the following:
-- - the empty list
-- - a cons list consisting of first element and the rest list
--                                                        ^^^^ self-reference
data List a =
    Empty
  | Cons a (List a)
  deriving Show

-- 2-element list: 5 7
list2 = Cons 5 (Cons 7 Empty)
-- 3-element list: 3 5 7
list3 = Cons 3 (Cons 5 (Cons 7 Empty))
list4 = Cons 11 list3

highway :: List Animal
highway = Cons dillo1 (Cons dillo2 (Cons parrot1 (Cons parrot2 Empty)))

runOverAnimals :: List Animal -> List Animal
runOverAnimals Empty = Empty
runOverAnimals (Cons first rest) =
    Cons (runOverAnimal first) (runOverAnimals rest)

feedAnimals1 :: List Animal -> List Animal
feedAnimals1 Empty = Empty
feedAnimals1 (Cons first rest) =
    Cons ((feedAnimal 1) first) (feedAnimals1 rest)

-- (algebraic structure: functor)

listMap :: (a -> b) -> List a -> List b
listMap process Empty = Empty
listMap process (Cons first rest) =
  Cons (process first) (listMap process rest)

-- add all numbers in list
listSum :: List Integer -> Integer
listSum Empty = 0
listSum (Cons first rest) =
    first + listSum rest


-- multiplies all numbers in list
listProduct :: List Integer -> Integer
listProduct Empty = 1 
listProduct (Cons first rest) =
    first * listProduct rest

-- associativity
-- (a + b) + c = a + (b + c)
-- (a * b) * c = a * (b * c)

-- 0 is the identity / neutral element of +
-- 0 + x = x + 0 = x
-- 1 is the identity / neutral element of *
-- 1 * x = x * 1 = x

-- group theory
-- geometry?

type Endo a = a -> a

g :: Endo Integer
g x = x + 1

h :: Endo Liveness
h Dead = Alive
h Alive = Dead

-- + :: Integer -> Integer -> Integer
-- * :: Integer -> Integer -> Integer

-- geometric function :: R2 -> R2 = Endo R2
 
-- neutral element wrt. combineEndo: identity function

identity :: p -> p
identity x = x

-- combineEndo a (combineEndo b c) = combineEnde (combineEndo a b) c
combineEndo :: Endo a -> Endo a -> Endo a
combineEndo f g = \ x -> g (f x)

-- append two lists
-- neutral element: Empty
-- append a (append b c) = append (append a b) c
append :: List a -> List a -> List a
append Empty list2 = list2

-- list1 = 1 2 3
-- list2 = 4 5 6
append (Cons first rest) list2 =
    -- first = 1
    -- append rest list2 = append (Cons 2 (Cons 3 Empty)) list2 = 2 3 4 5 6
    Cons first (append rest list2)


-- ingredients:
-- set / type t
-- binary operation / combinator 
-- op :: t -> t -> t
-- associativity: op a (op b c) = op (op a b) c
-- semigroup

-- semigroup + neutral element: monoid

-- class: TYPE CLASS, NOT OO class, more like interface
class Semigroup t where
    -- op must be associative
    op :: t -> t -> t

-- think "implementation"
instance Semigroup Integer where
    op = (+)

instance Semigroup (Endo a) where
    op = combineEndo

instance Semigroup (List a) where
    op = append

class Semigroup t => Monoid t where
    neutral :: t

instance Monoid Integer where
    neutral = 0

instance Monoid (Endo a) where
    neutral = identity

instance Monoid (List a) where
    neutral = Empty


-- How to create a domain model?

-- - ask domain expert for simple examples
-- Karthik: bond with maturity, payment after some time
-- Zero-Coupon Bond
-- "Receive 100GBP on Dec 24, 2021"

data Currency = GBP | EUR 
  deriving Show

data Date = Date String
  deriving (Show, Ord, Eq)

{-
data Contract =
    ZeroCouponBond Date Double Currency 
  | Future
  | CurrencySwap
  | AnnuityProduct
  | Everest
  | K2

-- code needs to be added on each new product
-- - lots of work
-- - bugs
-- - takes time

zcb1 = ZeroCouponBond (Date "2021-12-24") 100 GBP

-}

-- smash simple example into separate "atomic" ideas
--   while doing that: look for self-references

-- "Receive 100GBP on Dec 24, 2021"
-- - with maturity / "later"
-- - currency
-- - multiple ...

-- - ask for more examples

data Contract =
    -- "receive One EUR NOW"
    OneOf Currency
    -- "receive 100 EUR NOW"
  | Multiple Double Contract -- <- self-reference
  | WithMaturity Date Contract
  deriving Show

zcb1 :: Contract
zcb1 = WithMaturity (Date "2021-12-24") (Multiple 100 (OneOf GBP))

zeroCouponBond :: Date -> Double -> Currency -> Contract
zeroCouponBond maturity amount currency =
    WithMaturity maturity (Multiple amount (OneOf currency))

-- "smart constructor"
withMaturity :: Date -> Contract -> Contract
withMaturity date1 (WithMaturity date2 contract) =
    WithMaturity (max date1 date2) contract
withMaturity date contract = WithMaturity date contract


zcb1' :: Contract
zcb1' = zeroCouponBond (Date "2021-12-24") 100 GBP

