module Data.Traversable.Utils
( traverseAccumL
, traverseAccumR
) where

import Control.Applicative (liftA2)
import Data.Functor.Compose

newtype StateL s a = StateL { runStateL :: s -> (s, a) }

instance Functor (StateL s) where
    fmap f (StateL k) = StateL $ \s -> let (s', x) = k s in (s', f x)

instance Applicative (StateL s) where
    pure x = StateL $ \s -> (s, x)

    (StateL kf) <*> (StateL kx) = StateL $ \s ->
        let (s', f) = kf s
            (s'', x) = kx s'
        in (s'', f x)

    liftA2 f (StateL kx) (StateL ky) = StateL $ \s ->
        let (s', x) = kx s
            (s'', y) = ky s'
        in (s'', f x y)

newtype StateR s a = StateR { runStateR :: s -> (s, a) }

instance Functor (StateR s) where
    fmap f (StateR k) = StateR $ \s -> let (s', x) = k s in (s', f x)

instance Applicative (StateR s) where
    pure x = StateR $ \s -> (s, x)

    (StateR kf) <*> (StateR kx) = StateR $ \s ->
        let (s', x) = kx s
            (s'', f) = kf s'
        in (s'', f x)

    liftA2 f (StateR kx) (StateR ky) = StateR $ \s ->
        let (s', y) = ky s
            (s'', x) = kx s'
        in (s'', f x y)

traverseAccumL :: (Traversable t, Applicative f) => (a -> b -> (a, f c)) -> a -> t b -> (a, f (t c))
traverseAccumL f s t = runStateL (getCompose $ traverse (Compose . StateL . flip f) t) s

traverseAccumR :: (Traversable t, Applicative f) => (a -> b -> (a, f c)) -> a -> t b -> (a, f (t c))
traverseAccumR f s t = runStateR (getCompose $ traverse (Compose . StateR . flip f) t) s
