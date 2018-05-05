module Parser.Question
     ( parseQuestion
     ) where

import qualified Data.Text.Lazy as LT

import           Exceptions
import           Parser.Util    (parseCategory, parseLocale)
import           Types

-- | Parse description
parseQDesc' :: LT.Text -> Either KBError (LT.Text, LT.Text, LT.Text)
parseQDesc' txt = case LT.lines txt of
                      (_:question:_:locale:_:solution) -> return
                          (question, locale, LT.unlines solution)
                      _                                -> Left InvalidFormat

-- | Parse description
parseQDesc :: LT.Text -> Either KBError QDescription
parseQDesc txt = do
    (question, locale, solution) <- parseQDesc' txt
    parsedLocale <- parseLocale locale
    return $ QDescription parsedLocale question solution

-- | Parse question directory
parseQuestion :: [LT.Text] -> LT.Text -> Either KBError Question
parseQuestion ds metadata = do
    descriptions <- mapM parseQDesc ds
    category <- parseMetaDatas $ LT.lines metadata
    return       $ Question category descriptions

-- | Parse metadata
parseMetaDatas :: [LT.Text] -> Either KBError Category
parseMetaDatas (cat:_) = parseCategory cat
parseMetaDatas _       = Left InvalidFormat
