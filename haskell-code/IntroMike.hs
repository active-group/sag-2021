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
data List =
    Empty
  | Cons Integer List
  deriving Show

-- 2-element list: 5 7
list2 = Cons 5 (Cons 7 Empty)
-- 3-element list: 3 5 7
list3 = Cons 3 (Cons 5 (Cons 7 Empty))
list4 = Cons 11 list3

-- add all numbers in list
listSum Empty = 0
listSum (Cons first rest) =
    first + listSum rest