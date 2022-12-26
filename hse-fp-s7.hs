{-# LANGUAGE InstanceSigs #-}
ops :: Monoid m => m -> m -> m
ops a b = b <> a

x = ops "bbb" "aaa"

newtype Sum a = Sum { getSum :: a }

instance Num a => Semigroup (Sum a) where
  Sum a <> Sum b = Sum (a + b)

instance Num a => Monoid (Sum a) where
  mempty = Sum 0

sum' :: Num a => [a] -> a
sum' = getSum . foldMap Sum

rebuild :: [a] -> [a]
rebuild = foldMap (\x -> [x])

newtype Triple a = Tri (a, a, a)

instance Foldable Triple where
  foldMap :: Monoid m => (a -> m) -> Triple a -> m
  foldMap f (Tri (x, y, z)) = foldMap f [x, y, z]

genericSum :: (Foldable t, Num a) => t a -> a
genericSum = getSum . foldMap Sum

foldMapAsFoldr :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMapAsFoldr f = foldr (\a m -> f a <> m) mempty

newtype Endo b = Endo { runEndo :: b -> b }

instance Semigroup (Endo b) where
  Endo f <> Endo g = Endo (f . g)
  -- ^ forall f g h : (f . g) . h = f . (g . h)

instance Monoid (Endo b) where
  mempty = Endo id
  -- ^ forall f : f . id = id . f = f

foldrAsFoldMap :: Foldable t => (a -> b -> b) -> b -> t a -> b
foldrAsFoldMap f b xs = let aggr = foldMap (Endo . f) xs in runEndo aggr b

newtype Dual a = Dual { getDual :: a }

instance Semigroup a => Semigroup (Dual a) where
  (<>) :: Semigroup a => Dual a -> Dual a -> Dual a
  Dual x <> Dual y = Dual (y <> x)

instance Monoid a => Monoid (Dual a) where
  mempty = Dual mempty

rebuild' :: [a] -> [a]
rebuild' = getDual . foldMap (\x -> Dual [x])
-- foldMap (\x -> D [x]) [1, 2, 3, 4]
-- fold [D [1], D[2], D[3], D[4]]
-- foldr (<>) mempty [D[1], D[2], D[3], D[4]]
-- D[1] <> (D[2] <> (D[3] <> (D[4] <> mempty)))
-- ((([] ++ [4]) ++ [3]) ++ [2]) ++ [1]
--       [4, 3, 2, 1]
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
--       [4, 3, 2]
--  ^^^^^^^^^^^^^^^^^^^^
--       [4, 3]
--   ^^^^^^^^^^^
--       [4]
--    ^^
--    []

newtype First a = First { getFirst :: Maybe a }

instance Semigroup (First a) where
  First Nothing <> y = y
  x <> _ = x

instance Monoid (First a) where
  mempty = First Nothing
  -- ^ x <> First Nothing = x?

