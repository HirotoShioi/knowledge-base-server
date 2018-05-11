{-# LANGUAGE OverloadedStrings #-}

module Parser.Util
    ( Parser
    , Document(..)
    , parseCategory
    , parseLocale
    , parseMetadata
    ) where

import           RIO

import qualified RIO.Text as T

import           Exceptions
import           Types          (Category (..), Locale (..))

-- | Parse each category
parseCategory' :: Text -> Parser Category
parseCategory' "daedalus" = return Daedalus
parseCategory' "cardano"  = return Cardano
parseCategory' "backend"  = return Backend
parseCategory' "core"     = return Core
parseCategory' "network"  = return Network
parseCategory' err        = Left $ InvalidCategory err

parseCategory :: Text -> Parser Category
parseCategory str = case T.stripPrefix "category:" str of
                         Just cat -> parseCategory' $ T.toLower $ T.strip cat
                         Nothing  -> Left InvalidFormat

-- | Parse locale
parseLocale :: Text -> Parser Locale
parseLocale "en" = return En
parseLocale "ja" = return Ja
parseLocale err  = Left $ InvalidLocale err

-- | Parse metadata's field
parseMetadata :: Text -> Text -> Parser Text
parseMetadata err txt = case T.stripPrefix (err <> ":") (T.toLower txt) of
                            Just str -> return $ T.strip str
                            Nothing  -> Left InvalidFormat

type Parser a = Either KBError a

data Document = Document
    { docMetadata    :: !Text
    , docDescription :: ![Text]
    }
