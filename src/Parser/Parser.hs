module Parser.Parser
    ( Parser
    , Document(..)
    , parseKnowledge
    , parseFAQ
    ) where

import           Parser.Knowledge (parseKnowledge)
import           Parser.FAQ       (parseFAQ)
import           Parser.Util      (Parser, Document(..))
