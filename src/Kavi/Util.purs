module Kavi.Util where

import Data.Array (findIndex)

all :: forall a. (a -> Boolean) -> [a] -> Boolean
all f []           = true
all f (x:xs) | f x = all f xs
all f _            = false

contains :: forall a. (Eq a) => [a] -> a -> Boolean
contains xs x = if findIndex (\v -> v == x) xs == -1 then false else true
