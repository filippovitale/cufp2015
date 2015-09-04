module Example where

import Prelude hiding (add)

foreign import add :: Int -> Int -> String

identity :: forall a. a -> a
identity a = a

-- two = 1 + 1
two = add 1 1
