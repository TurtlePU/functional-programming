{-# LANGUAGE EmptyCase #-}

import Data.Function (on)
import Data.List (sortBy)
import Prelude hiding (foldr, scanr)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as L
import Data.Void (Void, absurd)

data List a = LNil | LCons a (List a)

ones = LCons 1 ones

data Tree a
    = Nil
    | Node { vertex :: a, left :: Tree a, right :: Tree a }
    deriving (Eq, Show)

myTree = Node { left = Node 'f' Nil Nil, vertex = 'o', right = Node 'o' Nil Nil }

myTree' = Node {
    left = Node { left = Node 'f' Nil Nil, vertex = 'o', right = Nil },
    vertex = 'o', right = Nil }

infiniteTree :: Tree Char
infiniteTree = Node {left = infiniteTree, vertex = 'a', right = infiniteTree}

otherTree :: Tree Char
otherTree = Node {left = otherTree, vertex = 'b', right = otherTree}

collect :: Tree a -> [a]
collect Nil = []
collect (Node x l r) = collect l ++ x : collect r

eqByContents :: Eq a => Tree a -> Tree a -> Bool
eqByContents = (==) `on` collect

treeSize = length . collect

sortedTreeList :: [Tree Int]
sortedTreeList = sortBy (compare `on` treeSize) [Nil, Node 1 Nil Nil]

newtype SelfApp a = MkSelfApp { runSelfApp :: SelfApp a -> a }

fixpoint :: (t -> t) -> t
fixpoint f = (\x -> f (runSelfApp x x)) (MkSelfApp (\x -> f (runSelfApp x x)))

fix' :: (t -> t) -> t
fix' f = f (fix' f)

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ x [] = x
foldr f x (y : ys) = f y (foldr f x ys)

scanr :: (a -> b -> b) -> b -> [a] -> NonEmpty b
-- ^ scanr f x [a, b, c] = [f a (f b (f c x)), f b (f c x), f c x, x]
scanr _ x [] = x L.:| []
scanr f x (y : ys) = let bs = scanr f x ys in f y (L.head bs) L.<| bs

scanr' :: (a -> b -> b) -> b -> [a] -> NonEmpty b
scanr' f x = foldr (\y bs -> f y (L.head bs) L.<| bs) (x L.:| [])

empty :: Void
empty = empty

empty' :: Void
empty' = head []

type VoidInt = (Void, Int)

absurd' :: Void -> a
absurd' x = case x of -- ???!

f :: Void -> VoidInt
f = absurd' -- ???

g :: VoidInt -> Void
g (v, x) = v

-- g . f = id :: Void -> Void
-- forall (x :: Void), g (f x) = x

-- f . g = id :: VoidInt -> VoidInt
-- forall (p :: VoidInt) f (g p) = p
-- forall (x :: Void, y :: Int) f (g (x, y)) = (x, y)
