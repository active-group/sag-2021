module IntroMike where

import Prelude hiding (Functor, Monad, Monoid, Semigroup)

x :: Integer
x = 12

f :: Integer -> Integer
f x = x + 5

-- Animals on the Texas highway

-- Armadillo has the following properties:
-- - dead or alive - AND -
-- - weight 

data Liveness = Dead | Alive
  deriving Show -- makes Liveness printable
-- data: new datatype, | = or

type Weight = Integer 
-- type synonym

-- "state of the animal at a certain time"
data Animal = Dillo Liveness Weight
  deriving Show

-- 
dillo1 :: Animal
dillo1 = Dillo Alive 10

dillo2 :: Animal
dillo2 = Dillo Dead 12

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
