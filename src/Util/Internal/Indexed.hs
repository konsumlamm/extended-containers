module Util.Internal.Indexed where

-- @Compose (State Int) f a@
newtype Indexed f a = Indexed { runIndexed :: Int -> (f a, Int) }

instance Functor f => Functor (Indexed f) where
    fmap f (Indexed sf) = Indexed $ \s -> let (x, s') = sf s in (fmap f x, s')

instance Applicative f => Applicative (Indexed f) where
    pure x = Indexed $ (,) (pure x)

    Indexed sfa <*> Indexed sfb = Indexed $ \s ->
        let (f, s') = sfa s
            (x, s'') = sfb s'
        in (f <*> x, s'')

evalIndexed :: Indexed f a -> Int -> f a
evalIndexed (Indexed sf) x = fst (sf x)