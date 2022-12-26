{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

import Control.Applicative (ZipList (ZipList, getZipList))

-- class Functor f => Applicative f where
--   pure :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b
--   (*>) :: f a -> f b -> f b
--   (*>) = liftA2 (\a b -> b)

fmapAsApp :: Applicative f => (a -> b) -> f a -> f b
fmapAsApp g fx = pure g <*> fx

liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 g fa fb = g <$> fa <*> fb

apply :: Applicative f => f (a -> b) -> f a -> f b
apply = liftA2 id

maybeFmap :: (a -> b) -> Maybe a -> Maybe b
maybeFmap f (Just x) = Just (f x)
maybeFmap _ _ = Nothing

maybePure :: a -> Maybe a
maybePure = Just

maybeApply :: Maybe (a -> b) -> Maybe a -> Maybe b
maybeApply (Just f) mx = maybeFmap f mx
maybeApply _ _ = Nothing

-- list #1

listPure :: a -> [a]
listPure x = [x]

listLiftA2 :: (a -> b -> c) -> [a] -> [b] -> [c]
listLiftA2 f xs ys = [f x y | x <- xs, y <- ys]

listBind :: [a] -> (a -> [b]) -> [b]
listBind [] f = []
listBind (x : xs) f = f x ++ listBind xs f

-- [ f x y | x <- xs, y <- ys ] = listBind xs (\x -> listBind ys (\y -> [f x y]))

-- list #2

listPure' :: a -> [a]
listPure' = repeat

listLiftA2' :: (a -> b -> c) -> [a] -> [b] -> [c]
listLiftA2' = zipWith

-- zipWith f xs ys = bind xs (\x -> bind ys (\y -> repeat (f x y)))

-- map f xs = zipWith id (listPure' f) xs

pairA :: Applicative f => f a -> f b -> f (a, b)
pairA = liftA2 (,)

pairs = getZipList $ pairA (ZipList [1, 2]) (ZipList "Hello")

ioApply :: IO (a -> b) -> IO a -> IO b
ioApply iof iox = do
  f <- iof
  f <$> iox

newtype Parser a = Parser {runParser :: String -> (Maybe a, String)}
  deriving (Functor)

instance Applicative Parser where
  pure x = Parser $ \s -> (Just x, s)
  Parser pf <*> Parser px = Parser $ \s ->
    case pf s of
      (Just f, s') -> let (mx, s'') = px s in (f <$> mx, s'')
      (Nothing, s') -> (Nothing, s')

char :: Parser Char
char = Parser $ \case
  [] -> (Nothing, [])
  (c : str) -> (Just c, str)

-- class Applicative m => Monad m where
--   return = pure
--   (>>=) :: m a -> (a -> m b) -> m b
--   (>>) :: m a -> m b -> m b

join :: Monad m => m (m a) -> m a
join mma = mma >>= id

bind :: Monad m => m a -> (a -> m b) -> m b
bind ma f = join (fmap f ma)

applyAsMonad :: Monad m => m (a -> b) -> m a -> m b
applyAsMonad mf mx = bind mf (\f -> f <$> mx)

fmapAsMonad :: Monad m => (a -> b) -> m a -> m b
fmapAsMonad f mx = bind mx (\x -> return (f x))

main :: IO ()
main = do
  putStrLn "Please enter your name:"
  name <- getLine
  putStrLn $ "Hello, " ++ name ++ "!"

main' = putStrLn _ >> (getLine >>= (\name -> putStrLn _))
