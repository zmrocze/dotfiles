{-# LANGUAGE RankNTypes #-}

data Free f a = Return a | Wrap (f (Free f a))

instance Functor f => Monad (Free f) where
    return = Return
    ma@(Return _) >>= _ = ma
    (Wrap fma) >>= mg = (\ma -> ma >>= (mg . ($)) ) <$> fma

-- Leafy trees are a special case, with F as the functor. Please write
-- functions which witness this isomorphism.

data F a = N a a


-- We now define an abstract version of arbitrary monads, analogous to
-- abstracted trees.  Witness an isomorphism.

newtype C m a = C { unC :: forall r. (a -> m r) -> m r }

rep :: Monad m => m a -> C m a
rep = undefined

abs :: Monad m => C m a -> m a
abs = undefined

-- Implement the monad instance from scratch, without rep/abs.

instance Monad (C m) where
    return = undefined
    (>>=)  = undefined -- also tricky; if you get stuck, look at the
                        -- implementation for CTrees
