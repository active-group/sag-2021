module IntroMike where

import Prelude hiding (Functor, Monad, Monoid, Semigroup)

x :: Integer
x = 12

-- Animals on the Texas highway

-- Armadillo has the following properties:
-- - dead or alive
-- - weight 

data Liveness = Dead | Alive
--  deriving Show -- makes Liveness printable
-- data: new datatype, | = or