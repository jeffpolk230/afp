module MyFunctors where

import Data.Char

-- | Mightbe 
data Mightbe a = Yes a | No

instance Functor Mightbe where
  fmap = mightFmap

-- | lift a given function into M f
mightFmap :: (a -> b) -> (Mightbe a -> Mightbe b)
mightFmap _ No = No
mightFmap f (Yes a) = Yes $ f a

-- | Signal from class
type Time = Double 
newtype Signal a = Sig { unSig :: Time -> a }

{-
  x = Sig $ \t -> 3 * t
  unSig x 10
-}
constS x = Sig (const x)
timeS = Sig id


instance Functor Signal where
  fmap = signalFmap

signalFmap :: ( a -> b) -> (Signal a -> Signal b)
signalFmap f (Sig a) = Sig $ \t -> f (a t)


{-
fmap   :: Functor f   => (a -> b) -> (f a -> f b)
cofmap :: Cofunctor f => (b -> a) -> (f a -> f b)
-}


newtype T a = T (a -> Int)

tCoFmap invF (T a) = T $ \b -> invF b

data Eitr a = L a | R a

eitrFmap f (R a) = R $ f a
eitrFmap _ (L a) = L a