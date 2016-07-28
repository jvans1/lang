module Base(
  module ClassyPrelude, 
  module Data.HashMap.Strict, 
  module Data.List.NonEmpty, 
  read
) where
import ClassyPrelude hiding (empty, try, many)
import Data.List.NonEmpty(NonEmpty)
import Prelude(read)
import Data.HashMap.Strict(insert, empty)

tshow :: Show a => a -> Text
tshow = pack . show
