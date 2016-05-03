{-# LANGUAGE MultiParamTypeClasses, Rank2Types, FlexibleContexts,
    ScopedTypeVariables #-}
module Flow where

import Control.Applicative
import Control.Monad.State.Strict

import Data.Map.Strict (Map, (!))
import Data.Set (Set)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- A recursive dataflow graph consists of a set of nodes, each with an initial
-- value, and a step function that computes the "next" value of a node from the
-- "previous" values of all nodes.
--
-- For this to be deterministic/confluent, I believe it suffices that the step
-- function be monotone (wrt some ordering of the value type) in the "previous
-- values" map!
data Graph node value = Graph { graphInit :: Init node value
                              , graphStep :: Step node value }

type Init node value = Map node value

-- The step function is restricted to be Applicative in the nodes' values. This
-- prevents dynamically choosing which nodes to evaluate based on a node's
-- value, forcing the dependency graph to be static. This prevents some useful
-- dataflow programs, but enables some useful implementation strategies; a
-- tradeoff.
type Step node value =
    forall exp. Applicative exp =>
    (node -> exp value) -> node -> exp value


-- Some utility functions.
tabulate :: Ord a => [a] -> (a -> b) -> Map a b
tabulate keys func = Map.fromList [(k, func k) | k <- keys]

invert :: (Ord a, Ord b) => Map a (Set b) -> Map b (Set a)
invert m = Map.fromListWith Set.union l
    where l = [ (v, Set.singleton k)
              | (k,vs) <- Map.toList m, v <- Set.toList vs]


--------------------------------------------------------
---------- PUSH-BASED dataflow implementation ----------
--------------------------------------------------------

-- The applicative expression functor we use.
data PushExp node value a = PushExp { pushDeps :: Set node
                                    , pushThunk :: Map node value -> a }

instance Ord node => Functor (PushExp node value) where fmap = liftA
instance Ord node => Applicative (PushExp node value) where
    pure x = PushExp Set.empty (const x)
    PushExp adeps a <*> PushExp bdeps b = PushExp (Set.union adeps bdeps) ab
        where ab map = a map (b map)

readNode :: Ord node => node -> PushExp node value value
readNode node = PushExp (Set.singleton node) (! node)

-- The monad in which we iterate the state to completion
type Push node value = State (Set node, Map node value)

pushFix :: forall node value. (Ord node, Eq value) =>
           Graph node value -> Map node value
pushFix (Graph init step) = evalState loop (Set.fromList nodes, init)
    where
      nodes = Map.keys init
      loop = do next <- popDirty
                case next of Just node -> do run node; loop
                             Nothing -> gets snd
      run :: node -> Push node value ()
      run node = do cache <- gets snd
                    let oldValue = cache ! node
                    let newValue = pushThunk (exprs ! node) cache
                    unless (oldValue == newValue) $ do
                      markDirty (clientsOf node)
                      writeNode node newValue
      exprs :: Map node (PushExp node value value)
      exprs = tabulate nodes (step readNode)
      -- needed in case a node has no clients and doesn't show up in the
      -- inverted dependency graph.
      clientsOf node = Map.findWithDefault Set.empty node clients
      clients :: Map node (Set node)
      clients = invert (Map.map pushDeps exprs)

writeNode :: (Ord node, Eq value) => node -> value -> Push node value ()
writeNode node value = modify f
    where f (dirty, cache) = (dirty, Map.insert node value cache)

markDirty :: (Ord node, Eq value) => Set node -> Push node value ()
markDirty nodes = modify (\(dirty, cache) -> (Set.union dirty nodes, cache))

popDirty :: (Ord node, Eq value) =>  Push node value (Maybe node)
popDirty = do (dirty, cache) <- get
              case choose dirty of
                Nothing -> return Nothing
                Just (node, dirty') -> do put (dirty', cache)
                                          return (Just node)

-- A scheduling strategy for dirty nodes. Currently: the one with the smallest
-- index. I'm not sure there's anything smarter that we could do.
choose :: Ord a => Set a -> Maybe (a, Set a)
choose x | Set.null x = Nothing
         | otherwise = Just (Set.deleteFindMin x)


--------------------------------------------------------
---------- PULL-BASED dataflow implementation ----------
--------------------------------------------------------

-- The "expression" type we use.
newtype PullExp node value a =
    PullExp { runPullExp :: Set node -> Map node value
                         -> (a, Bool, Set node, Map node value) }

-- Instances for PullExp.
instance Ord node => Functor (PullExp node value) where fmap = liftM
instance Ord node => Applicative (PullExp node value) where
    pure = return; (<*>) = ap
instance Ord node => Monad (PullExp node value) where
    return x = PullExp (\_ cache -> (x, False, Set.empty, cache))
    PullExp a >>= f = PullExp g
        where g frozen cache =
                  let (av, achange, avisit, cache1) = a frozen cache
                      frozen1 = Set.union frozen avisit
                      (fv, fchange, fvisit, cache2) =
                          runPullExp (f av) frozen1 cache1
                  in (fv, achange || fchange, Set.union avisit fvisit, cache2)

instance Ord node => MonadState (Map node value) (PullExp node value) where
    state f = PullExp (\_ cache -> let (v,cache') = f cache
                                   in (v, False, Set.empty, cache'))

markChanged :: PullExp node value ()
markChanged = PullExp (\frozen cache -> ((), True, Set.empty, cache))

markVisited :: Set node -> PullExp node value ()
markVisited nodes = PullExp (\frozen cache -> ((), False, nodes, cache))

getFrozen :: PullExp node value (Set node)
getFrozen = PullExp (\frozen cache -> (frozen, False, Set.empty, cache))

listen :: PullExp node value a -> PullExp node value (a, Bool, Set node)
listen (PullExp f) = PullExp g
    where g frozen cache = ((x, changed, visited), changed, visited, cache')
              where (x, changed, visited, cache') = f frozen cache

-- Our top-level state is: a set of finished nodes & a map from nodes to their
-- current values.
type PullState node value = (Set node, Map node value)

pullInit :: Ord node => Graph node value -> PullState node value
pullInit g = (Set.empty, graphInit g)

-- The guts of the pull-based implementation are here.
pullGet :: forall node value m.
           (Ord node, Eq value, MonadState (PullState node value) m) =>
           Graph node value -> node -> m value
pullGet graph node = do (finished, cache) <- get
                        let (value, _changed, visited, cache') =
                                runPullExp (visit node) finished cache
                        put (Set.union finished visited, cache')
                        return value
    where
      visit :: node -> PullExp node value value
      visit node = do cachedValue <- gets (! node)
                      -- we have to get the frozen set before we markVisited,
                      -- b/c markVisited adds to the frozen set.
                      frozen <- getFrozen
                      markVisited (Set.singleton node)
                      if Set.member node frozen
                      then return cachedValue
                      else iterate node cachedValue
      iterate :: node -> value -> PullExp node value value
      iterate node oldValue = do
        (newValue, changed, visited) <- listen (step node oldValue)
        if not changed || not (Set.member node visited)
        -- if we didn't change or we didn't depend on ourselves, no need to
        -- iterate further.
        then return newValue
        else iterate node newValue
      step :: node -> value -> PullExp node value value
      step node oldValue = do newValue <- graphStep graph visit node
                              when (oldValue /= newValue) $ do
                                modify (Map.insert node newValue)
                                markChanged
                              return newValue


------------------------------
---------- Examples ----------
------------------------------
testPull :: (Ord n, Eq v) => Graph n v -> n -> v
testPull g n = evalState (pullGet g n) state
    where state = pullInit g

ex1 :: Graph Int Int
ex1 = Graph
      (Map.fromList $ zip [0..10] [0,0..])
      (\self n -> case n of 0 -> self 0
                            n -> (1 +) <$> self (n-1))

ex2 :: Graph Int Int
ex2 = Graph
      (Map.fromList $ zip [0..10] [0,0..])
      (\self n -> case n of
                    0 -> let f x = if x < 2 then x+1 else x
                         in f <$> self n
                    n -> (1+) <$> self (n-1))
