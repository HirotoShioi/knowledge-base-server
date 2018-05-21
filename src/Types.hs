{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Types
    ( Category(..)
    , FAQ
    , FAQDescription
    , KnowledgeDescription
    , Knowledge
    , Locale(..)
    , Output
    ) where

import           RIO

import           Data.Aeson (ToJSON)
import           Data.Extensible
import           GHC.Generics (Generic)
import           RIO.Time (UTCTime)

-- | Defining Locale
data Locale
    = En  -- ^ English
    | Ja  -- ^ Japanese
    deriving (Show, Generic)

-- | Defininng Category
data Category
    = Daedalus  -- ^ Frontend
    | Cardano   -- ^ Cardano
    | Backend   -- ^ Backend/wallet
    | Core      -- ^ Core
    | Network   -- ^ Network
    deriving (Show, Generic)

type KnowledgeDescription = Record
    '[ "locale"   >: Locale  -- ^ Locale of the description
     , "problem"  >: Text    -- ^ Text describing the problem
     , "solution" >: Text    -- ^ Text describing solution
     ]

type Knowledge = Record
    '[ "errorCode"    >: Text
     , "category"     >: Category
     , "errorText"    >: Text
     , "descriptions" >: [KnowledgeDescription]
     ]

type FAQDescription = Record
    '[ "locale"   >: Locale
     , "question" >: Text
     , "solution" >: Text
     ]

type FAQ = Record
    '[ "category"     >: Category
     , "descriptions" >: [FAQDescription]
     ]

-- | Data in which server returns when server API is called.
type Output a = Record
    '[ "timestamp"       >: UTCTime
     , "data"            >: [a]
     , "numberOfOutputs" >: Int
     ]

instance ToJSON Locale
instance ToJSON Category