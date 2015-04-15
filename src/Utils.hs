module Utils where

import Debug.Trace (trace)

partialLift :: Monad m => (a -> b -> c) -> a -> m b -> m c
partialLift f a mb = do b <- mb
                        return $ f a b

pairwise :: (a -> c, b -> d) -> (a, b) -> (c, d)
pairwise (f, g) (a, b) = (f a, g b)

pmap :: (a -> c, b -> d) -> [(a, b)] -> [(c, d)]
pmap = map . pairwise

unwrapPair :: Monad m => (a, m b) -> m (a, b)
unwrapPair (a, mb) = do b <- mb
                        return (a, b)

showTrace :: Show a => a -> a
showTrace = trace =<< show

