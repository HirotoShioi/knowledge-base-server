{-# LANGUAGE OverloadedStrings #-}

module Parser.Knowledge
    ( parseKnowledge
    ) where

import qualified Data.Text.Lazy as LT

import           Exceptions
import           Parser.Util    (parseCategory, parseLocale, parseMetadata)
import           Types          (Category, KDescription (..), Knowledge (..))

-- | Parse description
parseKDesc' :: LT.Text -> Either KBError (LT.Text, LT.Text, LT.Text)
parseKDesc' txt = case LT.lines txt of
                      (_:problem:_:locale:_:solution) ->
                          return (problem, locale, LT.unlines solution)
                      _                               -> Left InvalidFormat
-- | Parse description
parseKDesc :: LT.Text -> Either KBError KDescription
parseKDesc txt = do
    (problem, locale, solution) <- parseKDesc' txt
    parsedLocale <- parseLocale locale
    return $ KDescription parsedLocale problem solution

-- | Parse knowledge's meta data
parseKnowledgeMeta :: LT.Text -> Either KBError (LT.Text, Category, LT.Text)
parseKnowledgeMeta txt = case LT.lines txt of
                             (errCode:ecat:etxt:_) -> do
                                  errorCategory <- parseCategory ecat
                                  errorCode     <- parseMetadata "errorcode" errCode
                                  errorText     <- parseMetadata "errortext" etxt
                                  return (errorCode, errorCategory, errorText)
                             _               -> Left InvalidFormat

-- | Parse knowledge directory
parseKnowledge :: [LT.Text] -> LT.Text -> Either KBError Knowledge
parseKnowledge ds metadata = do
    descriptions <- mapM parseKDesc ds
    (eCode, eCategory, eText) <- parseKnowledgeMeta metadata
    return $ Knowledge eCode eCategory eText descriptions
