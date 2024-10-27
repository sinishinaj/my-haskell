import Control.Monad.IO.Class
import Data.Kind (Type)
import Control.Applicative.Lift (Lift(Pure))

class MyFunctor (f :: Type -> Type) where
  myFmap :: (a -> b) -> f a -> f b
    
(!$!) :: MyFunctor f => (a -> b) -> f a -> f b
(!$!) = myFmap

data MyMaybe a = MyNothing | MyJust a deriving Show

instance MyFunctor MyMaybe where
  myFmap :: (a -> b) -> MyMaybe a -> MyMaybe b
  myFmap func MyNothing = MyNothing
  myFmap func (MyJust x) = MyJust (func x)

class (MyFunctor f) => MyApplicative f where
  myPure :: a -> f a
  (!*!) :: f (a -> b) -> f a -> f b

instance MyApplicative MyMaybe where
  myPure :: a -> MyMaybe a
  myPure = MyJust
  (!*!) :: MyMaybe (a -> b) -> MyMaybe a -> MyMaybe b
  (!*!) MyNothing _ = MyNothing
  (!*!) (MyJust func) mX = myFmap func mX

a :: MyMaybe Int
a = MyJust 2

b :: MyMaybe Int
b = MyJust 3

pureFunc :: Int -> Int
pureFunc = (+1)

x :: MyMaybe Int
x = pureFunc !$! a

y :: MyMaybe Int
y = (*2) !$! a

z :: MyMaybe (Int -> Int)
z = myPure (+2)

w :: MyMaybe Int
w = z !*! b

w' :: MyMaybe Int
w' = myPure (*) !*! a !*! b

w'' :: Int
w'' = (*) 2 3

w''' :: MyMaybe Int
w''' = (*) !$! a !*! b

class MyMonad m where
  (!>>=!) :: m a -> (a -> m b) -> m b
  -- (!>>!) :: m a -> m b -> m b
  myReturn :: a -> m a

instance MyMonad MyMaybe where
  (!>>=!) :: MyMaybe a -> (a -> MyMaybe b) -> MyMaybe b
  (!>>=!) MyNothing _ = MyNothing
  (!>>=!) (MyJust x) func = func x
  myReturn :: a -> MyMaybe a
  myReturn = myPure

class MySemigroup a where
  (!<>!) :: a -> a -> a

class MySemigroup a => MyMonoid a where
  myMempty :: a 

newtype AdditionInt = AdditionInt Int
  deriving (Show, Eq)

instance MySemigroup AdditionInt where
  (!<>!) :: AdditionInt -> AdditionInt -> AdditionInt
  AdditionInt x !<>! AdditionInt y = AdditionInt (x + y)

g :: (MySemigroup a, Num a) => a -> a
g n = myMempty !<>! n

d :: MyMaybe Int
d = myPure 2

e :: Int -> MyMaybe Int
e n = (n+) !$! MyJust 3

r :: MyMaybe Int
r = d !>>=! e

f :: Int -> MyMaybe Int
f n = MyNothing

r' :: MyMaybe Int
r' = (d !>>=! f) !>>=! e !>>=! e

d' :: Maybe Int
d' = pure 2

e' :: Int -> Maybe Int
e' n = (n+) <$> Just 3

f' :: Int -> Maybe Int
f' n = Nothing

r'' :: Int -> Maybe Int
r'' n = (pure n >>= f') >>= e' >>= e'

r''' :: Int -> Maybe Int
r''' n = do
  x <- e' n
  y <- e' x
  z <- e' y
  return z