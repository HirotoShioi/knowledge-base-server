{-# LANGUAGE OverloadedStrings #-}

module Parser.Util
    ( parseCategory
    , parseLocale
    , parseMetadata
    ) where

import           Data.Semigroup ((<>))
import qualified Data.Text.Lazy as LT

import           Exceptions
import           Types         (Category(..), Locale(..))

-- | Parse each category
parseCategory' :: LT.Text -> Either KBError Category
parseCategory' "daedalus" = return Daedalus
parseCategory' "cardano"  = return Cardano
parseCategory' "backend"  = return Backend
parseCategory' "core"     = return Core
parseCategory' "network"  = return Network
parseCategory' err        = Left $ InvalidCategory err

parseCategory :: LT.Text -> Either KBError Category
parseCategory str = case LT.stripPrefix "category:" str of
                         Just cat -> parseCategory' $ LT.toLower $ LT.strip cat
                         Nothing  -> Left InvalidFormat

-- | Parse locale
parseLocale :: LT.Text -> Either KBError Locale
parseLocale "en" = return En
parseLocale "ja" = return Ja
parseLocale err  = Left $ InvalidLocale err

-- | Parse metadata's field
parseMetadata :: LT.Text -> LT.Text -> Either KBError LT.Text
parseMetadata err txt = case LT.stripPrefix (err <> ":") (LT.toLower txt) of
                            Just str -> return $ LT.strip str
                            Nothing  -> Left InvalidFormat
