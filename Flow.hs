{-# LANGUAGE MultiParamTypeClasses, RankNTypes, FlexibleContexts,
    ScopedTypeVariables, LambdaCase #-}
module Flow where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader.Class
import Control.Monad.State.Strict

import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- A recursive dataflow graph
type Init node value = Map node value
type Step node value =
    forall exp. Applicative exp =>
    (node -> exp value) -> node -> exp value

data Graph node value = Graph { graphInit :: Init node value
                              , graphStep :: Step node value }


-- Pull-based dataflow implementation

-- The top-level monad: we keep a set of finished nodes & a map from nodes to
-- their current values.
type PullState node value = (Set node, Map node value)

-- The "expression" type we use.
newtype Pull node value a =
    Pull { runPull :: Set node -> Map node value
                   -> (a, Bool, Set node, Map node value) }

-- Instances for Pull.
instance Ord node => Functor (Pull node value) where fmap = liftM
instance Ord node => Applicative (Pull node value) where
    pure = return; (<*>) = ap
instance Ord node => Monad (Pull node value) where
    return x = Pull (\_ cache -> (x, False, Set.empty, cache))
    Pull a >>= f = Pull g
        where g frozen cache =
                  let (av, achange, avisit, cache1) = a frozen cache
                      frozen1 = Set.union frozen avisit
                      (fv, fchange, fvisit, cache2) =
                          runPull (f av) frozen1 cache1
                  in (fv, achange || fchange, Set.union avisit fvisit, cache2)

instance Ord node => MonadState (Map node value) (Pull node value) where
    state f = Pull (\_ cache -> let (v,cache') = f cache
                                in (v, False, Set.empty, cache'))

instance Ord node => MonadReader (Set node) (Pull node value) where
    ask = Pull (\frozen cache -> (frozen, False, Set.empty, cache))
    local f (Pull g) = Pull (g . f)

markChanged :: Pull node value ()
markChanged = Pull (\frozen cache -> ((), True, Set.empty, cache))

markVisited :: Set node -> Pull node value ()
markVisited nodes = Pull (\frozen cache -> ((), False, nodes, cache))

listen :: Pull node value a -> Pull node value (a, Bool, Set node)
listen (Pull f) = Pull g
    where g frozen cache = ((x, changed, visited), changed, visited, cache')
              where (x, changed, visited, cache') = f frozen cache


-- The guts of the pull-based dataflow implementation
pullInit :: Ord node => Graph node value -> PullState node value
pullInit g = (Set.empty, graphInit g)

pullGet :: forall node value m.
           (Ord node, Eq value, MonadState (PullState node value) m) =>
           Graph node value -> node -> m value
pullGet graph node = do (finished, cache) <- get
                        let (value, _changed, visited, cache') =
                                runPull (visit node) finished cache
                        put (Set.union finished visited, cache')
                        return value
    where
      visit :: node -> Pull node value value
      visit node = do cachedValue <- gets (Map.! node)
                      frozen <- ask
                      markVisited (Set.singleton node)
                      frozen2 <- ask
                      unless (Set.member node frozen2) (error "THIS SHOULD NOT HAPPEN")
                      if Set.member node frozen
                      then return cachedValue
                      else local (Set.union (Set.singleton node))
                                 (iterate node cachedValue)
      iterate :: node -> value -> Pull node value value
      iterate node oldValue = do
        (newValue, changed, visited) <- listen (step node oldValue)
        if not changed || not (Set.member node visited)
        -- if we didn't change or we didn't depend on ourselves, no need to
        -- iterate further.
        then return newValue
        else iterate node newValue
      step :: node -> value -> Pull node value value
      step node oldValue = do newValue <- graphStep graph visit node
                              when (oldValue /= newValue) $ do
                                modify (Map.insert node newValue)
                                markChanged
                              return newValue


-- Push-based dataflow implementation
-- data Push st node a = Push { pushDeps :: Set node
--                            , pushThunk :: ST st a }



-- Examples
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
