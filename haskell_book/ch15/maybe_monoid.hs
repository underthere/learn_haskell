import           Data.Monoid

data Optional a = Nada
                | Only a
                deriving (Eq, Show)


instance (Semigroup a) => Semigroup (Optional a) where
  (<>) (Only a) (Only b) = Only ( a <> b)
  (<>) Nada (Only b) = Only b
  (<>) (Only a) Nada = Only a
  (<>) Nada Nada = Nada

instance (Monoid a) => Monoid (Optional a) where
  mempty                    = Nada
