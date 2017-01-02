
fmap'' _ [] = []
fmap'' g (x:xs) = g x : g x : fmap'' g xs

class Functor f => Pointed f where
  clean :: a -> f a

instance Pointed Maybe where
  clean x = Just x

instance Pointed ((->) e) where
  clean x = (\_ -> x)

data Perhaps a = Honest a
               | Aught
  deriving (Show)

instance Functor Perhaps where
  fmap g (Honest x) = Honest (g x)
  fmap _ Aught = Aught

instance Applicative Perhaps where
  pure x = Honest x
  _ <*> Aught = Aught
  Aught <*> _  = Aught
  (Honest g) <*> (Honest x) = Honest (g x)

newtype ZipList a = ZipList { getZipList :: [a] }
  deriving (Show)

instance Functor ZipList where
  fmap _ (ZipList []) = ZipList []
  fmap f  (ZipList (x:xs)) = ZipList (f x:(fmap f xs))

instance Applicative ZipList where
  pure x = ZipList [x]
  (ZipList gs) <*> (ZipList xs) = ZipList (zipWith ($) gs xs)

class Applicative m => Monad'' m where
  join :: m (m a) -> m a

instance Monad'' Maybe where
  join mmx = mmx >>= \mx -> mx
