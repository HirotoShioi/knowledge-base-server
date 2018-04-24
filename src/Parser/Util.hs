{-# LANGUAGE OverloadedStrings #-}

module Parser.Util
    (
      parseCategory
    , parseLocale
    ) where

import qualified Data.Text.Lazy as LT

import           Exceptions
import           Types

-- | Parse each category
parseCategory :: LT.Text -> Either KBError Category
parseCategory "Daedalus" = return Daedalus
parseCategory "Cardano"  = return Cardano
parseCategory "Backend"  = return Backend
parseCategory "Core"     = return Core
parseCategory "Network"  = return Network
parseCategory err        = Left $ InvalidCategory err

-- | Parse locale
parseLocale :: LT.Text -> Either KBError Locale
parseLocale "en" = return En
parseLocale "ja" = return Ja
parseLocale err   = Left $ InvalidLocale err
