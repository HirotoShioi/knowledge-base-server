module Parser.FAQ
     ( parseFAQ
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
parseQDesc :: LT.Text -> Either KBError FAQDescription
parseQDesc txt = do
    (question, locale, solution) <- parseQDesc' txt
    parsedLocale <- parseLocale locale
    return $ QDescription parsedLocale question solution

-- | Parse FAQ directory
parseFAQ :: [LT.Text] -> LT.Text -> Either KBError FAQ
parseFAQ ds metadata = do
    descriptions <- mapM parseQDesc ds
    category <- parseFAQMetaDatas $ LT.lines metadata
    return       $ FAQ category descriptions

-- | Parse metadata
parseFAQMetaDatas :: [LT.Text] -> Either KBError Category
parseFAQMetaDatas (cat:_) = parseCategory cat
parseFAQMetaDatas _       = Left InvalidFormat
