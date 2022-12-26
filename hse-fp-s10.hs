{-# LANGUAGE DeriveFunctor #-}

import Control.Applicative (liftA2)
import Control.Monad (ap, join)
import Control.Category (Category)
import qualified Control.Category as C

-- class Applicative m => Monad m where
--   return :: a -> m a
--   (>>=) :: m a -> (a -> m b) -> m b

-- data Maybe a = Just a | Nothing

maybeBind :: Maybe a -> (a -> Maybe b) -> Maybe b
maybeBind Nothing _ = Nothing
maybeBind (Just a) f = f a

safeDiv :: Int -> Int -> Maybe Int
safeDiv x y
  | y == 0 = Nothing
  | otherwise = Just (x `div` y)

pipeline :: Int -> Int -> Int -> Int -> Maybe Int
-- ^ pipeline x y z w = (x / y) / (z / w)
pipeline x y z w = join $ liftA2 safeDiv (safeDiv x y) (safeDiv z w)

newtype Writer s a = Writer {runWriter :: (a, s)} deriving (Functor)

mapWriter :: (s -> s') -> Writer s a -> Writer s' a
mapWriter f (Writer (x, s)) = Writer (x, f s)

execWriter :: Writer s a -> a
execWriter = fst . runWriter

evalWriter :: Writer s a -> s
evalWriter = snd . runWriter

instance Monoid s => Applicative (Writer s) where
  pure x = Writer (x, mempty)
  (<*>) = ap

instance Monoid s => Monad (Writer s) where
  Writer (x, s) >>= f = Writer (y, s <> s')
    where
      Writer (y, s') = f x

-- pure x >>= f = f x
-- m >>= pure = m

hello :: Writer String ()
hello = Writer ((), "hello!\n")

program :: Writer String Int
program = do
  hello
  let result = 5 + 5
  hello
  pure result

newtype Reader e a = Reader {runReader :: e -> a} deriving (Functor)

mapReader :: (e' -> e) -> Reader e a -> Reader e' a
mapReader f (Reader x) = Reader (\e' -> x (f e'))

instance Applicative (Reader e) where
  pure a = Reader (const a)
  (<*>) = ap

instance Monad (Reader e) where
  Reader x >>= f = Reader (\e -> runReader (f (x e)) e)

ask :: Reader e e
ask = Reader id

program' :: Reader e (e, e)
program' = do
  x <- ask
  pure (x, x)

newtype State s a = State {runState :: s -> (a, s)} deriving (Functor)

evalState :: State s a -> s -> a
evalState sa s = fst $ runState sa s

execState :: State s a -> s -> s
execState sa s = snd $ runState sa s

modifyState :: (s -> s) -> State s ()
modifyState upd = State $ \s -> ((), upd s)

getState :: State s s
getState = State $ \s -> (s, s)

putState :: s -> State s ()
putState s' = State $ const ((), s')

-- s = Int
-- t = (Int, Char)
-- fst :: t -> s
-- (Int, Char) -> Int -> (Int, Char) :: t -> s -> t

data Lens a b = Lens {to :: a -> b, from :: a -> b -> a}

instance Category Lens where
  id = lensId
  (.) = (>>>>)

lensId :: Lens a a
lensId = Lens id (\_ x -> x)

(>>>>) :: Lens b c -> Lens a b -> Lens a c
Lens to' from' >>>> Lens aToB aFromB =
  Lens
    { to = to' . aToB,
      from = \a c -> aFromB a $ from' (aToB a) c
    }

mapState :: (t -> s) -> (t -> s -> t) -> State s a -> State t a
mapState f g (State x) = State $ \t -> let (a, s') = x (f t) in (a, g t s')

mapState' :: Semigroup t => (t -> s) -> (s -> t) -> State s a -> State t a
mapState' f g = mapState f (\t s -> t <> g s)

instance Applicative (State s) where
  pure a = State (\s -> (a, s))
  (<*>) = ap

instance Monad (State s) where
  State x >>= f = State $ \s ->
    let (a, s') = x s
     in let State y = f a
         in y s'

main = print $ runWriter program
