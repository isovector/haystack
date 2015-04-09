module Utils where

partialLift :: Monad m => (a -> b -> c) -> a -> m b -> m c
partialLift f a mb = do b <- mb
                        return $ f a b

pairwise :: (a -> c, b -> d) -> (a, b) -> (c, d)
pairwise (f, g) (a, b) = (f a, g b)

pmap :: (a -> c, b -> d) -> [(a, b)] -> [(c, d)]
pmap = map . pairwise
