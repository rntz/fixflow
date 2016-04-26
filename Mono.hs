{-# LANGUAGE MultiParamTypeClasses, RankNTypes #-}
module Mono
    ( Mono, runMono, const, unwrap )
where

import Prelude hiding (map, const)

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.List as List

class Ord a => Pointed a where least :: a
class Pointed a => Semilattice a where lub :: a -> a -> a

instance Ord a => Pointed (Set a) where least = Set.empty
instance Ord a => Semilattice (Set a) where lub = Set.union


-- Monotone expressions
newtype Mono a = Mono a deriving (Show, Eq, Ord)

runMono :: Mono a -> IO a
runMono (Mono x) = return x

const :: a -> Mono a
const = Mono

unwrap :: Mono (Mono a) -> Mono a
unwrap (Mono x) = x

-- Booleans
when :: Pointed a => Mono Bool -> Mono a -> Mono a
when (Mono True) x  = x
when (Mono False) _ = Mono least

-- Pairs
pair :: (Mono a, Mono b) -> Mono (a,b)
pair (Mono x, Mono y) = Mono (x,y)

unpair :: Mono (a,b) -> (Mono a, Mono b)
unpair (Mono x) = (Mono (fst x), Mono (snd x))

fstM = fst . unpair
sndM = snd . unpair

-- Semilattices
instance Pointed a => Pointed (Mono a) where least = Mono least
instance Semilattice a => Semilattice (Mono a) where
    lub (Mono x) (Mono y) = Mono (lub x y)

-- Sets
bind :: (Ord a, Semilattice b) => Mono (a -> b) -> Mono (Set a) -> Mono b
bind (Mono f) (Mono a) = Mono $ Set.foldl' accum least a
    where accum elt acc = lub elt (f acc)

map :: (Ord a, Ord b) => Mono (a -> b) -> Mono (Set a) -> Mono (Set b)
map (Mono f) (Mono x) = Mono (Set.map f x)


-- Fixed points by iteration
-- not guaranteed to terminate, unfortunately
fix :: Semilattice a => (Mono a -> Mono a) -> Mono a
fix f = iter least (f least)
    where iter prev next | prev == next = prev
                         | otherwise = iter next (f next)


-- Monotone primitives
(+^) :: Mono Int -> Mono Int -> Mono Int
Mono x +^ Mono y = Mono (x + y)

(-^) :: Mono Int -> Int -> Mono Int
Mono x -^ y = Mono (x - y)
