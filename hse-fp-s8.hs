import Control.Concurrent.Async (forConcurrently)
import Control.Monad.Random (Random (randomR), initStdGen)
import Data.Foldable (traverse_)
import Data.Functor.Identity (Identity (Identity))

listMap :: (a -> b) -> [a] -> [b]
listMap = fmap

tupleMap :: (a -> b) -> (c, a) -> (c, b)
tupleMap = fmap

idMap :: (a -> b) -> Identity a -> Identity b
idMap = fmap

ioMap :: (a -> b) -> IO a -> IO b
ioMap = fmap

maybeMap :: (a -> b) -> Maybe a -> Maybe b
-- ^ maybeMap id m = m ?
-- ^ maybeMap (f . g) m = maybeMap f (maybeMap g m)
maybeMap f Nothing = Nothing
maybeMap f (Just a) = Just (f a)

funMap :: (a -> b) -> (e -> a) -> (e -> b)
-- ^ funMap id f = id . f = f ?
-- ^ funMap (g . h) f = (g . h) . f = g . (h . f) = funMap g (funMap h f) ?
funMap = (.)

revMap :: (b -> a) -> (a -> r) -> b -> r
-- ^ (b -> a) -> (f a -> f b)
revMap f g b = g (f b)

data Point = Point Float Float

getRandomPoint :: Bounds -> IO Point
getRandomPoint b = do
  g <- initStdGen
  let (x, g') = randomR (minX b, maxX b) g
  let (y, _) = randomR (minY b, maxY b) g'
  return (Point x y)

data Bounds = Bounds {minX :: Float, maxX :: Float, minY :: Float, maxY :: Float}

monteCarlo :: (Point -> Bool) -> Bounds -> Int -> IO Float
monteCarlo f b n = do
  attempts <- forConcurrently (replicate n b) $ \b -> do
    p <- getRandomPoint b
    pure $ if f p then 1 else 0
  let successes = sum attempts
  return (successes / fromIntegral n * boxSize b)
  where
    boxSize b = (maxX b - minX b) * (maxY b - minY b)

main1 :: IO ()
main1 = do
  n <- readLn
  size <- monteCarlo (\(Point x y) -> x ^ 2 + y ^ 2 < 1) (Bounds (-1) 1 (-1) 1) n
  print size

main :: IO ()
main = do
  text <- getContents
  let ls = lines text
  traverse_ putStrLn ls
