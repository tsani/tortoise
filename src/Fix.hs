module Fix where

newtype Fix f = Fix { unFix :: f (Fix f) }
type Alg f a = f a -> a

cata :: Functor f => Alg f a -> Fix f -> a
cata phi = phi . fmap (cata phi) . unFix
