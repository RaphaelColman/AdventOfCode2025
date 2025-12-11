module Common.EitherUtils where

swapEither :: Either a b -> Either b a
swapEither (Left x) = Right x
swapEither (Right y) = Left y

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left x) = Left (f x)
mapLeft _f (Right y) = Right y
