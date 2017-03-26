{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Fix where

newtype Fix f = Fix { unFix :: f (Fix f) }

deriving instance Eq (f (Fix f)) => Eq (Fix f)
deriving instance Ord (f (Fix f)) => Ord (Fix f)
deriving instance Read (f (Fix f)) => Read (Fix f)
deriving instance Show (f (Fix f)) => Show (Fix f)



type Alg f a = f a -> a

cata :: Functor f => Alg f a -> Fix f -> a
cata phi = phi . fmap (cata phi) . unFix
