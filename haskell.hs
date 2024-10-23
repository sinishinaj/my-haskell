class MyFunctor f where
    mymap :: (a -> b) -> f a -> f b
    
instance MyFunctor [] where
    mymap = map

data MyMaybe a = MyNothing | MyJust a deriving Show

instance MyFunctor MyMaybe where
  mymap :: (a -> b) -> MyMaybe a -> MyMaybe b
  mymap func MyNothing = MyNothing
  mymap func (MyJust x) = MyJust (func x)

class (MyFunctor f) => MyApplicative f where
  mypure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

instance MyApplicative MyMaybe where
  mypure :: a -> MyMaybe a
  mypure = MyJust
  (<*>) :: MyMaybe (a -> b) -> MyMaybe a -> MyMaybe b
  (<*>) MyNothing _ = MyNothing
  (<*>) (MyJust func) mX = mymap func mX
