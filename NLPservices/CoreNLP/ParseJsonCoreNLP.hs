-----------------------------------------------------------------------------
--
-- Module      :  parsing the output of stanford corenlp 3.9. in json format
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}

{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
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

-- all data has 2 suffix

--parseNLP :: ErrIO ()
--parseNLP = do
--    f :: LazyByteString <- readFile2 (makeRelFile "short1 .json")
--    let r = decodeDoc2 f -- :: Maybe [Doc2]
--    putIOwords ["decoded", showT r]
--    return ()

decodeDoc2 :: LazyByteString -> Either String Doc2
--decodeDoc2 :: ByteString -> Either String Doc2
decodeDoc2 = eitherDecode

data Doc2 = Doc2 {doc_sentences::  [Sentence2]
                  , doc_corefs :: Maybe Coreferences2-- [CorefChain2]
                       } deriving (Read, Show,  Eq, Ord, Generic)

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
                        } deriving (Read, Show,  Eq, Ord, Generic)

instance FromJSON Sentence2 where
    parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop 2 } . fieldlabels2filtered

fieldlabels2filtered  (Object o) = Object . HM.fromList . map filterLabel . HM.toList $ o
    where
        filterLabel :: (Text, Value) -> (Text, Value)
        filterLabel (key,value) = (filterChar (/='-') key, value)
fieldlabels2filtered x = x

data Dependency2 = Dependency2 {dep_dep ::  Text -- the tag
                        , dep_governor :: Int
                        , dep_governorGloss :: Text
                        , dep_dependent :: Int
                        , dep_dependentGloss :: Text
                        } deriving (Read, Show,  Eq, Ord, Generic)

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
                } deriving (Read, Show,  Eq, Ord, Generic)

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
                } deriving (Read, Show,  Eq, Ord, Generic)

instance FromJSON Token2 where
    parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop 4 }

data Coreferences2 = Coreferences2  {chains:: [CorefChain2] }
                 deriving (Read, Show,  Eq, Ord, Generic)

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
                 deriving (Read, Show,  Eq, Ord, Generic, FromJSON)

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
                } deriving (Read, Show,  Eq, Ord, Generic)

instance FromJSON Coref2 where
    parseJSON =   genericParseJSON opts
        where
          opts = defaultOptions { fieldLabelModifier =  drop 6 }
--
