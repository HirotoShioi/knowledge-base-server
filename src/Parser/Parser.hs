module Parser.Parser
    ( Parser
    , Document(..)
    , parseKnowledge
    , parseFAQ
    ) where

import           Parser.FAQ (parseFAQ)
import           Parser.Knowledge (parseKnowledge)
import           Parser.Util (Document (..), Parser)
