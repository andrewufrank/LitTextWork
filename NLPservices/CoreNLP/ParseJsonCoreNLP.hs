-----------------------------------------------------------------------------
--
-- Module      :  parsing the output of stanford corenlp 3.9. in json format
-- produces Doc2
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}

{-# LANGUAGE DeriveGeneric, DeriveAnyClass, StandaloneDeriving #-}
--{-# LANGUAGE TemplateHaskell #-}
-- template haskell requires reordering of data types
--and all functions used for default otions imported (not local defined)

module CoreNLP.ParseJsonCoreNLP -- (openMain, htf_thisModuelsTests)
     where

import           Uniform.Strings
import Uniform.FileIO

import Data.Aeson
import Data.Aeson.Types  -- for modifying the labels
import GHC.Generics
import Control.Monad (mzero)
import qualified Data.HashMap.Strict as HM
import LitTypes.LanguageTypedText (LTtext(..), LanguageTypedText(..))
--import NLP2RDF.NLPvocabulary (SnipSigl(..))
import NLP.Types.Tags (POStags(..))
import Data.RDFext.Extension -- (Triple)  -- instance Show Triple
import CoreNLP.Vocabulary
import LitTypes.LanguageTypedText (unLCtext, LCtext (..))
-- all data has 2 suffix ??

-- | a single language piece of text with lanuage code
-- , length and start para number
data Snip2 lang = Snip2 { snip2text :: LTtext lang
                        , snip2sigl :: SnipSigl  -- the id of the snip
                          }
            deriving (Read, Show, Eq)
instance Zeros (Snip2 lang) where
    zero = Snip2 zero zero

snipIsNull :: Snip2 lang -> Bool
-- ^ test for null text
snipIsNull = null' . unLCtext . snip2text

newtype NLPtriple postag = NLPtriple Triple deriving (Eq, Ord, Show, Read)
unNLPtriple (NLPtriple t) = t


--processDoc0toTriples2 :: (Show postag, POStags postag, LanguageTypedText lang)
--            => lang ->  postag -> SnipSigl  -> Doc0 postag -> [NLPtriple postag]
--            -- TriplesGraph  G -> H
---- ^ convert the doc0 (which is the analysed xml) and produce the triples
--
--processDoc0toTriples2  lph pph snipsigl  doc0 =
--    map NLPtriple $ t2  : sents -- ++ corefs
--
--    where
--        lang = languageCode lph -- tz3lang ntz
----        snipid = snip2sigl snip -- mkSnipSigl paraid snipnr
--        t2 = mkTripleText (unSnipSigl snipsigl)
--                (mkRDFproperty LanguageTag) (showT lang)
--        sents :: [Triple]
--        sents =   concat $ map (mkSentenceTriple2 lang  snipsigl) (docSents doc0)
----        corefs =    (mkCorefTriple1 lang   snipsigl )
----                            (docCorefs doc0)

decodeDoc2 :: LazyByteString -> Either String Doc2
--decodeDoc2 :: ByteString -> Either String Doc2
decodeDoc2 = eitherDecode

data Doc2 = Doc2 {doc_sentences::  [Sentence2]
                  , doc_corefs :: Maybe Coreferences2-- [CorefChain2]
                       }
           deriving (Show, Read, Eq, Ord, Generic, ToJSON)

instance FromJSON Doc2 where
    parseJSON = genericParseJSON doc2ops
doc2ops = defaultOptions {
                fieldLabelModifier = drop 4 }

data Sentence2 = Sentence2 {s_index :: Int
                , s_parse :: Maybe Text  -- the parse tree
                , s_basicDependencies :: Maybe [Dependency2]
                , s_enhancedDependencies :: Maybe [Dependency2]
                , s_enhancedPlusPlusDependencies :: Maybe [Dependency2]
                , s_collapse_ccprocessed_dependencies :: Maybe [Dependency2]
                        -- collapsed-ccprocessed-dependencies
                , s_entitymentions :: Maybe [Ner2]
                , s_tokens :: [Token2]
                }
        deriving (Show, Read, Eq, Ord, Generic, ToJSON)

instance FromJSON Sentence2 where
    parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop 2 } . fieldlabels2filtered

fieldlabels2filtered  (Object o) = Object . HM.fromList
                . map filterLabel . HM.toList $ o
    where
        filterLabel :: (Text, Value) -> (Text, Value)
        filterLabel (key,value) = (filterChar (/='-') key, value)
fieldlabels2filtered x = x

data Dependency2 = Dependency2 {dep_dep ::  Text -- the tag
                        , dep_governor :: Int
                        , dep_governorGloss :: Text
                        , dep_dependent :: Int
                        , dep_dependentGloss :: Text
                        }
                deriving (Show, Read, Eq, Ord, Generic, ToJSON, Zeros)

instance FromJSON Dependency2 where
    parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop 4 }

data Ner2 = Ner2 {ner_docTokenBegin :: Int
                , ner_docTokenEnd :: Int
                , ner_tokenBegin :: Int
                , ner_tokenEnd :: Int
                , ner_text :: Text
                , ner_characterOffsetBegin :: Int
                , ner_characterOffsetEnd :: Int
                , ner_ner :: Text -- the code
                }
        deriving (Show, Read, Eq, Ord, Generic, ToJSON, Zeros)

instance FromJSON Ner2 where
    parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop 4 }

data Token2 = Token2 {tok_index :: Int
                , tok_word :: Text
                , tok_originalText :: Text
                , tok_lemma :: Text
                , tok_characterOffsetBegin :: Int
                , tok_characterOffsetEnd :: Int
                , tok_pos :: Text
                , tok_ner :: Text  -- missing NormalizedNER ?
                , tok_speaker :: Maybe Text
                , tok_before :: Maybe Text
                , tok_after :: Maybe Text
                } deriving (Show, Read, Eq, Ord, Generic, ToJSON, Zeros)
instance FromJSON Token2 where
    parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop 4 }

data Coreferences2 = Coreferences2  {chains:: [CorefChain2] }
                 deriving (Show, Read, Eq, Ord, Generic, ToJSON, Zeros)

instance FromJSON Coreferences2 where
    parseJSON =   genericParseJSON opts  . jsonToArray
        where
          opts = defaultOptions

---- convert fields into array -- applied before the parse of Coreferences2
jsonToArray :: Value -> Value
--jsonToArray = id
jsonToArray (Object vals) = -- error . show $
    object ["chains" .= (fmap snd . HM.toList $ vals) ]
jsonToArray x = x


data CorefChain2 = CorefChain2 [Coref2]
         deriving (Read, Show,  Eq, Ord, Generic, ToJSON, FromJSON, Zeros)

--instance FromJSON CorefChain2 where


data Coref2 = Coref2 {coref_id :: Int
                    , coref_text :: Text
--                    , coref_type :: Text
--                    , coref_number :: Text
--                    , coref_gender :: Text
--                    , coref_animacy :: Text
                    , coref_startIndex :: Int
                    , coref_endIndex :: Int
                    , coref_headIndex :: Int
                    , coref_sentNum :: Int
--                    , coref_position :: [Int]
                    , coref_isRepresentativeMention :: Bool
                } deriving (Show, Read, Eq, Ord, Generic, ToJSON, Zeros)
instance FromJSON Coref2 where
    parseJSON =   genericParseJSON opts
        where
          opts = defaultOptions { fieldLabelModifier =  drop 6 }
--
