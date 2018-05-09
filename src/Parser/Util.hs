{-# LANGUAGE OverloadedStrings #-}

module Parser.Util
    ( Parser
    , Document(..)
    , parseCategory
    , parseLocale
    , parseMetadata
    ) where

import           Universum

import qualified Data.Text.Lazy as LT

import           Exceptions
import           Types          (Category (..), Locale (..))

-- | Parse each category
parseCategory' :: LText -> Parser Category
parseCategory' "daedalus" = return Daedalus
parseCategory' "cardano"  = return Cardano
parseCategory' "backend"  = return Backend
parseCategory' "core"     = return Core
parseCategory' "network"  = return Network
parseCategory' err        = Left $ InvalidCategory err

parseCategory :: LText -> Parser Category
parseCategory str = case LT.stripPrefix "category:" str of
                         Just cat -> parseCategory' $ LT.toLower $ LT.strip cat
                         Nothing  -> Left InvalidFormat

-- | Parse locale
parseLocale :: LText -> Parser Locale
parseLocale "en" = return En
parseLocale "ja" = return Ja
parseLocale err  = Left $ InvalidLocale err

-- | Parse metadata's field
parseMetadata :: LText -> LText -> Parser LText
parseMetadata err txt = case LT.stripPrefix (err <> ":") (LT.toLower txt) of
                            Just str -> return $ LT.strip str
                            Nothing  -> Left InvalidFormat

type Parser a = Either KBError a

data Document = Document
    { docMetadata    :: !LText
    , docDescription :: ![LText]
    }
