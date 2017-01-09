module Game where

-- part 02
import Control.Monad.Free

-- executable design doc
data Contract next =
  Move  MoveFlavor Position next
  Where (Position -> next)

-- make Contract a functor so our Free commands can map over it
instance Functor Contract where
  fmap f (Move mf p n) = Move mf p $ f n
  fmap f (Where g)  = Where  $ f . g

-- a free list of contract agreements as functors
type WalkingSim = Free Contract ()

-- dependency types
newtype Position   = Position Int Int Int
data    MoveFlavor = Saunter | Amble | Sprint
