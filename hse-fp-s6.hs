{-# LANGUAGE LambdaCase #-}

type Ambig = Maybe Bool

maybe' :: b -> (a -> b) -> Maybe a -> b
maybe' x _ Nothing = x
maybe' _ f (Just a) = f a

zipMaybe :: Maybe a -> Maybe b -> Maybe (a, b)
zipMaybe (Just x) (Just y) = Just (x, y)
zipMaybe _ _ = Nothing

(&&&) :: Ambig -> Ambig -> Ambig
Just True &&& Just True = Just True
Just False &&& _ = Just False
_ &&& Just False = Just False
_ &&& _ = Nothing

not' :: Ambig -> Ambig
not' = fmap not

partition :: [Either a b] -> ([a], [b])
-- ^ partition [Left x, Right y, Left z, Left w, Right x'] = ([x, z, w], [y, x'])
partition = foldMap $
    \case
        Left a -> ([a], [])
        Right b -> ([], [b])

-- instance Semigroup Ordering where
--    LT <> _ = LT
--    EQ <> y = y
--    GT <> _ = GT
-- instance Monoid Ordering where
--    mempty             = EQ
--
-- foldMap id [LT, GT, GT, LT, EQ] = LT
--             --
-- foldMap id (repeat GT) = GT
-- foldMap id [EQ, EQ, LT] = LT

lenCompare :: [a] -> [b] -> Ordering
lenCompare [] [] = EQ
lenCompare [] _ = LT
lenCompare _ [] = GT
lenCompare (_ : xs) (_ : ys) = lenCompare xs ys

lexCompare :: Ord a => [a] -> [a] -> Ordering
lexCompare xs ys = foldMap (uncurry compare) (zip xs ys) <> lenCompare xs ys

init' :: [a] -> [a]
init' [x] = []
init' (x : xs) = x : init' xs
init' [] = error "empty list"

init'' :: [a] -> [a]
init'' [] = error "empty list"
init'' xs = take (length xs - 1) xs

-- Maybe Integer ~ Integer

f :: Integer -> Maybe Integer
f 0 = Nothing
f n
  | n > 0 = Just (n - 1)
  | otherwise = Just n

g :: Maybe Integer -> Integer
g Nothing = 0
g (Just n)
  | n >= 0 = n + 1
  | otherwise = n

-- f (g x) = x:
-- f (g Nothing) = f 0 = Nothing
-- f (g (Just n)) = f (n + 1) = Just n (n >= 0)
-- f (g (Just n)) = f n = Just n (n < 0)

-- g (f x) = x :: Integer
-- ...