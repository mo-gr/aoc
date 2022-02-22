module AStar  (aStar,aStarM) where
-- adapted from https://github.com/weissi/astar/blob/master/src/Data/Graph/AStar.hs
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))
import Data.List (foldl')
import Control.Monad (foldM)

data AStar a c = AStar { visited  :: !(Set a),
                         waiting  :: !(Map a c),
                         score    :: !(Map a c),
                         memoHeur :: !(Map a c),
                         cameFrom :: !(Map a a),
                         end      :: !(Maybe a) }
    deriving Show

aStarInit :: Num c => k -> AStar k c
aStarInit start = AStar { visited  = Set.empty,
                          waiting  = Map.singleton start 0,
                          score    = Map.singleton start 0,
                          memoHeur = Map.empty,
                          cameFrom = Map.empty,
                          end      = Nothing }

runAStar :: (Ord a, Ord c, Num c)
         => (a -> Set a)     -- adjacencies in graph
         -> (a -> a -> c) -- distance function
         -> (a -> c)      -- heuristic distance to goal
         -> (a -> Bool)   -- goal
         -> a             -- starting vertex
         -> AStar a c     -- final state

runAStar graph dist heur goal start = aStar' (aStarInit start)
  where aStar' s
          = case Map.minViewWithKey (waiting s) of
              Nothing            -> s
              Just ((x,_), w') ->
                if goal x
                  then s { end = Just x }
                  else aStar' $ foldl' (expand x)
                                       (s { waiting = w',
                                            visited = Set.insert x (visited s)})
                                       (Set.toList (graph x `Set.difference` visited s))
        expand x s y
          = let v = score s ! x + dist x y
            in case Map.lookup y (waiting s) of
                 Nothing -> link x y v
                              (s { memoHeur
                                     = Map.insert y (heur y) (memoHeur s) })
                 Just _  -> if v < score s ! y
                              then link x y v s
                              else s
        link x y v s
           = s { cameFrom = Map.insert y x (cameFrom s),
                 score    = Map.insert y v (score s),
                 waiting  = Map.insert y (v + memoHeur s ! y) (waiting s) }

-- | This function computes an optimal (minimal distance) path through a graph in a best-first fashion,
-- starting from a given starting point.
aStar :: (Ord a, Ord c, Num c) =>
         (a -> Set a)     -- ^ The graph we are searching through, given as a function from vertices
                          -- to their neighbours.
         -> (a -> a -> c) -- ^ Distance function between neighbouring vertices of the graph. This will
                          -- never be applied to vertices that are not neighbours, so may be undefined
                          -- on pairs that are not neighbours in the graph.
         -> (a -> c)      -- ^ Heuristic distance to the (nearest) goal. This should never overestimate the
                          -- distance, or else the path found may not be minimal.
         -> (a -> Bool)   -- ^ The goal, specified as a boolean predicate on vertices.
         -> a             -- ^ The vertex to start searching from.
         -> Maybe [a]     -- ^ An optimal path, if any path exists. This excludes the starting vertex.
aStar graph dist heur goal start
    = let s = runAStar graph dist heur goal start
      in case end s of
            Nothing -> Nothing
            Just e  -> Just (reverse . takeWhile (/= start) . iterate (cameFrom s !) $ e)

runAStarM :: (Monad m, Ord a, Ord c, Num c) =>
          (a -> m (Set a))   -- adjacencies in graph
          -> (a -> a -> m c) -- distance function
          -> (a -> m c)      -- heuristic distance to goal
          -> (a -> m Bool)   -- goal
          -> a               -- starting vertex
          -> m (AStar a c)   -- final state

runAStarM graph dist heur goal start = aStar' (aStarInit start)
  where aStar' s
          = case Map.minViewWithKey (waiting s) of
              Nothing            -> return s
              Just ((x,_), w') ->
                do g <- goal x
                   if g then return (s { end = Just x })
                        else do ns <- graph x
                                u <- foldM (expand x)
                                           (s { waiting = w',
                                                visited = Set.insert x (visited s)})
                                           (Set.toList (ns `Set.difference` visited s))
                                aStar' u
        expand x s y
          = do d <- dist x y
               let v = score s ! x + d
               case Map.lookup y (waiting s) of
                 Nothing -> do h <- heur y
                               return (link x y v (s { memoHeur = Map.insert y h (memoHeur s) }))
                 Just _  -> return $ if v < score s ! y
                                        then link x y v s
                                        else s
        link x y v s
           = s { cameFrom = Map.insert y x (cameFrom s),
                 score    = Map.insert y v (score s),
                 waiting  = Map.insert y (v + memoHeur s ! y) (waiting s) }

-- | This function computes an optimal (minimal distance) path through a graph in a best-first fashion,
-- starting from a given starting point.
aStarM :: (Monad m, Ord a, Ord c, Num c) =>
         (a -> m (Set a))   -- ^ The graph we are searching through, given as a function from vertices
                            -- to their neighbours.
         -> (a -> a -> m c) -- ^ Distance function between neighbouring vertices of the graph. This will
                            -- never be applied to vertices that are not neighbours, so may be undefined
                            -- on pairs that are not neighbours in the graph.
         -> (a -> m c)      -- ^ Heuristic distance to the (nearest) goal. This should never overestimate the
                            -- distance, or else the path found may not be minimal.
         -> (a -> m Bool)   -- ^ The goal, specified as a boolean predicate on vertices.
         -> m a             -- ^ The vertex to start searching from.
         -> m (Maybe [a])   -- ^ An optimal path, if any path exists. This excludes the starting vertex.
aStarM graph dist heur goal start
    = do sv <- start
         s <- runAStarM graph dist heur goal sv
         return $ case end s of
                    Nothing -> Nothing
                    Just e  -> Just (reverse . takeWhile (/= sv) . iterate (cameFrom s !) $ e)
