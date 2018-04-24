{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module API
    (
      Knowledgebase
    , api
    ) where

import           Servant

import           Types

-- | Define an APi
type Knowledgebase =
       "knowledges" :> Get '[JSON] (Output Knowledge)
  :<|> "questions"  :> Get '[JSON] (Output Question)

api :: Proxy Knowledgebase
api = Proxy
