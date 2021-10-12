module IntroMike where

import Prelude hiding (Functor, Monad, Monoid, Semigroup)

x :: Integer
x = 12

-- Animals on the Texas highway

-- Armadillo has the following properties:
-- - dead or alive
-- - weight 

data Liveness = Dead | Alive
  deriving Show -- makes Liveness printable
-- data: new datatype, | = or

type Weight = Integer 
-- type synonym

data Animal = Dillo Liveness Weight
  deriving Show

-- 
dillo1 :: Animal
dillo1 = Dillo Alive 10

dillo2 :: Animal
dillo2 = Dillo Dead 12