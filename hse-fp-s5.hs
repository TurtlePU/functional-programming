import Prelude hiding (cycle)

newtype BFun = MkBFun { runBFun :: Bool -> Int }

instance Num BFun where
  MkBFun f + MkBFun g = MkBFun (\b -> f b + g b)
--   (*) = _
--   negate = _
  abs = MkBFun . (abs .) . runBFun
  signum = MkBFun . (signum .) . runBFun
  -- abs x * signum x = x
  -- (abs x * signum x) b = x b
  -- (abs x) b * (signum x) b = x b
  -- abs (x b) * signum (x b) = x b
  fromInteger = MkBFun . const . fromInteger

tails :: [a] -> [[a]]
-- ^ tails [] = [[]]
-- ^ tails [a, b, c] = [[a, b, c], [b, c], [c], []]
-- ^ tails [a1, a2, ...] = [[a1, a2, ...], [a2, a3, ...], [a3, a4, ...], ...]
tails [] = [[]]
tails ys@(_ : xs) = ys : tails xs

main :: IO ()
main = do
    line <- getLine
    let nums = map read (words line) :: [Int]
    putStr
        . unlines
        . map (unwords . map show . (1 :))
        $ tails nums

-- 1 : [1,
-- 1 :  2,
-- 1 :  3,
-- 1 :  4,
-- 1 : 66,
-- 1 : eps]

nub :: Eq a => [a] -> [a]
-- ^ nub [1, 2, 3] = [1, 2, 3]
-- ^ nub [1, 1, 2, 3] = [1, 2, 3]
-- ^ nub [1, 2, 3, 1] = [1, 2, 3, 1]
-- ^ nub [1, 1, 1] = [1]
nub [] = []
nub [x] = [x]
nub (x : y : xs)
  | x == y = nub (x : xs)
  | otherwise = x : nub (y : xs)

cyclicShiftL :: [a] -> [a]
cyclicShiftL (x : xs) = xs ++ [x]
cyclicShiftL [] = []

cyclicShiftR :: [a] -> [a]
cyclicShiftR = reverse . cyclicShiftL . reverse

cycle :: [a] -> [a]
-- ^ cycle "hello" = "hellohellohellohello..."
-- ^ cycle [] = [] ++ [] ++ [] ++ ...
cycle xs = xs ++ cycle xs