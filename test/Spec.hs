{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

import RIO

import Data.Extensible
import Test.Hspec

import Parser.Parser
import Types

import Prelude (head)

sampleMetaData :: Text
sampleMetaData = "category:Daedalus"

sampleFAQ :: [Text]
sampleFAQ = ["\nSample question\n\nen\n\nSample answer"]

sampleFAQDocument :: Document
sampleFAQDocument = #metadata     @= sampleMetaData
                 <: #descriptions @= sampleFAQ
                 <: nil

getFAQ :: Document -> FAQ
getFAQ doc =
  let parsedDocument = parseFAQ doc
      faqData = case parsedDocument of
                    Left _  -> error "Parse error"
                    Right d -> d
  in faqData

getFAQdescription :: Document -> FAQDescription
getFAQdescription doc = head $ getFAQ doc ^. #descriptions

main :: IO ()
main = hspec $ do
  describe "FAQ parser" $ do
    it "Should parse metadata" $
        getFAQ sampleFAQDocument ^. #category `shouldBe` Daedalus
    it "Should parse locale" $
        getFAQdescription sampleFAQDocument ^. #locale `shouldBe` En
    it "Should parse question" $
        getFAQdescription sampleFAQDocument ^. #question `shouldBe` "Sample question"
    it "Should parse solution" $
        getFAQdescription sampleFAQDocument ^. #solution `shouldBe` "Sample answer\n"
  describe "Knowledge parser" $
    it "Should parse medadata" pending
