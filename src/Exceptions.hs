module Exceptions
    ( KBError(..)
    ) where

import           RIO

import           Data.Typeable (Typeable)

-- | Exception message describing each exceptions
data KBError =
      EmptyDirectory
    -- ^ Empty Directory
    | ParseError KBError FilePath
    -- ^ Parsing Error
    | InvalidCategory Text
    -- ^ Invalid category
    | InvalidFormat
    -- ^ Invalid format in the markdown
    | InvalidLocale Text
    -- ^ Invalid locale is being written down
    deriving (Show, Typeable)

instance Exception KBError
