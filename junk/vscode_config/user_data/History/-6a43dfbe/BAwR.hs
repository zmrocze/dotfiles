
data ExceptA f a
    = f (Either a) 

instance Functor f => 