module Base(
  module ClassyPrelude,
  module Data.HashMap.Strict,
  module Data.List.NonEmpty,
  module Debug.Trace,
  read
) where
import ClassyPrelude hiding (empty, try, many, head)
import Debug.Trace(trace)
import Data.List.NonEmpty(NonEmpty(..), head)
import Prelude(read)
import Data.HashMap.Strict(insert, empty)

tshow :: Show a => a -> Text
tshow = pack . show
