{-# LANGUAGE RankNTypes #-}

module Optics
    ( Lens, Lens', Prism, Prism', Traversal, Traversal', Iso, Iso'
    , lens, prism, prism', iso
    , _1, _2
    , _Left, _Right, _Just, _Nothing, _Nil, _Cons
    , each, both
    , re, review, view, preview, over, set, toListOf
    ) where

import Data.Functor.Const     (Const (..))
import Data.Functor.Identity  (Identity (..))
import Data.Monoid            (First (..))
import Data.Profunctor        (Profunctor (..))
import Data.Profunctor.Choice (Choice (..))
import Data.Tagged            (Tagged (..))

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Prism s t a b = forall f p. (Applicative f, Choice p) => p a (f b) -> p s (f t)
type Traversal s t a b = forall f. Applicative f => (a -> f b) -> (s -> f t)
type Iso s t a b = forall p f. (Choice p, Functor f) => p a (f b) -> p s (f t)

type Lens' s a = Lens s s a a
type Prism' s a = Prism s s a a
type Traversal' s a = Traversal s s a a
type Iso' s a = Iso s s a a

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens gt st f s = st s <$> f (gt s)

prism :: (s -> Either t a) -> (b -> t) -> Prism s t a b
prism pr rv p = dimap pr (either pure (fmap rv)) (right' p)

prism' :: (s -> Maybe a) -> (a -> s) -> Prism' s a
prism' pr = prism pr'
  where
    pr' s = case pr s of
        Nothing -> Left s
        Just a  -> Right a

iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso sa as = dimap sa (fmap as)

_1 :: Lens (a, b) (a', b) a a'
_1 = lens fst (\(_, b) a -> (a, b))

_2 :: Lens (a, b) (a, b') b b'
_2 = lens snd (\(a, _) b -> (a, b))

_Left :: Prism (Either a b) (Either a' b) a a'
_Left = prism (either Right (Left . Right)) Left

_Right :: Prism (Either a b) (Either a b') b b'
_Right = prism (either (Left . Left) Right) Right

_Just :: Prism (Maybe a) (Maybe b) a b
_Just = prism (maybe (Left Nothing) Right) Just

_Nothing :: Prism' (Maybe a) ()
_Nothing = prism' (maybe (Just ()) (const Nothing)) (const Nothing)

_Cons :: Prism [a] [b] (a, [a]) (b, [b])
_Cons = prism p r
  where
    p []       = Left []
    p (x : xs) = Right (x, xs)

    r = uncurry (:)

_Nil :: Prism' [a] ()
_Nil = prism' p (const [])
  where
    p []      = Just ()
    p (_ : _) = Nothing

each :: Traversable t => Traversal (t a) (t b) a b
each = traverse

both :: Traversal (a, a) (b, b) a b
both f (a, a') = (,) <$> f a <*> f a'

re :: Iso s t a b -> Iso b a t s
re sa = iso (review sa) (view sa)

review :: (Tagged a (Identity b) -> Tagged s (Identity t)) -> b -> t
review p = runIdentity . unTagged . p . Tagged . Identity

over :: ((a -> Identity b) -> s -> Identity t) -> (a -> b) -> s -> t
over sa f s = runIdentity (sa (Identity . f) s)

view :: ((a -> Const a b) -> s -> Const a t) -> s -> a
view sa s = getConst (sa Const s)

toListOf :: ((a -> Const [a] a) -> s -> Const [a] s) -> s -> [a]
toListOf p = getConst . p (Const . return)

preview :: ((a -> Const (First a) a) -> s -> Const (First a) s) -> s -> Maybe a
preview p = getFirst . getConst . p (Const . First . Just)

set :: ((a -> Identity b) -> s -> Identity t) -> s -> b -> t
set sa s a = over sa (const a) s
