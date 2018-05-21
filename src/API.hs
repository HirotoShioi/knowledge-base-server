{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module API
    ( Knowledgebase
    , api
    ) where

import           Servant

import           Types (FAQ, Knowledge, Output)

-- | Define an APi
type Knowledgebase =
         "knowledges" :> Get '[JSON] (Output Knowledge)
    :<|> "faq"  :> Get '[JSON] (Output FAQ)

api :: Proxy Knowledgebase
api = Proxy
