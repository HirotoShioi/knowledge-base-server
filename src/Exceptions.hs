module Exceptions where

import           Control.Exception.Safe
import qualified Data.Text.Lazy         as LT
import           Data.Typeable          (Typeable)

-- | Exception message describing each exceptions
data KBError =
      ParseError KBError FilePath -- ^ Parsing Error
    | InvalidCategory LT.Text     -- ^ Invalid category
    | InvalidFormat               -- ^ Invalid format in the markdown
    | InvalidLocale LT.Text       -- ^ Invalid locale is being written down
    deriving (Show, Typeable)

instance Exception KBError
