{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Parser.Knowledge
    ( parseKnowledge
    ) where

import           RIO

import           Data.Extensible
import qualified RIO.Text as T

import           Exceptions
import           Parser.Util (Document, Parser, parseCategory, parseLocale, parseMetadata)
import           Types (Category, KnowledgeDescription, Knowledge)

-- | Parse description
parseKDesc' :: Text -> Parser (Text, Text, Text)
parseKDesc' txt = case T.lines txt of
    (_:problem:_:locale:_:solution) ->
        return (problem, locale, T.unlines solution)
    _                               -> Left InvalidFormat

-- | Parse description
parseKDesc :: Text -> Parser KnowledgeDescription
parseKDesc txt = do
    (problem, locale, solution) <- parseKDesc' txt
    parsedLocale <- parseLocale $ T.strip locale
    return $ #locale   @= parsedLocale
          <: #problem  @= problem
          <: #solution @= solution
          <: nil

-- | Parse knowledge's meta data
parseKnowledgeMeta :: Text -> Parser (Text, Category, Text)
parseKnowledgeMeta txt = case T.lines txt of
    (errCode:ecat:etxt:_) -> do
        errorCategory <- parseCategory ecat
        errorCode     <- parseMetadata "errorcode" errCode
        errorText     <- parseMetadata "errortext" etxt
        return (errorCode, errorCategory, errorText)
    _               -> Left InvalidFormat

-- | Parse knowledge directory
parseKnowledge :: Document -> Parser Knowledge
parseKnowledge doc = do
    descriptions <- mapM parseKDesc (doc ^. #descriptions)
    (eCode, eCategory, eText) <- parseKnowledgeMeta (doc ^. #metadata)
    return $ #errorCode    @= eCode
          <: #category     @= eCategory
          <: #errorText    @= eText
          <: #descriptions @= descriptions
          <: nil
