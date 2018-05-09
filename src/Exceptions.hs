module Exceptions
    ( KBError(..)
    ) where

import           Universum

-- | Exception message describing each exceptions
data KBError =
      EmptyDirectory
    | ParseError KBError FilePath -- ^ Parsing Error
    | InvalidCategory LText     -- ^ Invalid category
    | InvalidFormat               -- ^ Invalid format in the markdown
    | InvalidLocale LText       -- ^ Invalid locale is being written down
    deriving (Show)

instance Exception KBError
