{-# LANGUAGE DeriveGeneric #-}

module Types
    ( Category(..)
    , KDescription(..)
    , Knowledge(..)
    , Locale(..)
    , Output(..)
    , FAQDescription(..)
    , FAQ(..)
    ) where

import           RIO

import           Data.Aeson   (ToJSON)
import           GHC.Generics (Generic)
import           RIO.Time     (UTCTime)

-- | Defining Locale
data Locale
    = En -- ^ English
    | Ja -- ^ Japanese
    deriving (Show, Generic)

-- | Defininng Category
data Category
    = Daedalus -- ^ Frontend
    | Cardano  -- ^ Cardano
    | Backend  -- ^ Backend/wallet
    | Core     -- ^ Core
    | Network  -- ^ Network
    deriving (Show, Generic)

-- | Description for Knowledge
data KDescription = KDescription
    { dLocale   :: !Locale  -- ^ Locale of the description
    , dProblem  :: !Text -- ^ Text describing the problem
    , dSolution :: !Text -- ^ Text describing solution
    } deriving (Show, Generic)

-- | Knowledge needed to perform analysis
data Knowledge = Knowledge
    { kErrorCode   :: !Text       -- ^ Errorcode of the issue
    , kCategory    :: !Category      -- ^ Category of the issue
    , kErrorText   :: !Text       -- ^ Text in which can be used to analyze log file
    , kDescription :: [KDescription] -- ^ Descrption of the issue
    } deriving (Show, Generic)

-- | Description for FAQ
data FAQDescription = QDescription
    { qLocale  :: !Locale  -- ^ Locale of the description
    , qQuetion :: !Text -- ^ Question text
    , qAnswer  :: !Text -- ^ Answer to the question
    } deriving (Show, Generic)

-- | FAQ about Cardano, Daedalus
data FAQ = FAQ
    { faqCategory    :: !Category       -- ^ Category of the FAQ
    , faqDescription :: ![FAQDescription] -- ^ Description for the FAQ
    } deriving (Show, Generic)

-- | Data in which server returns when server API is called.
data Output a = Output
    { oTimestamp :: !UTCTime -- ^ Timestamp of when the api was called
    , oData      :: ![a]     -- ^ Either list of knowledge or FAQs
    , oNum       :: !Int     -- ^ Number of data
    } deriving (Show, Generic)

-- Todo: Define better instance.. (current json is not formatted nicely)
instance ToJSON Locale where
instance ToJSON KDescription where
instance ToJSON Knowledge where
instance ToJSON Category where
instance ToJSON FAQDescription where
instance ToJSON FAQ where
instance ToJSON a => ToJSON (Output a) where
