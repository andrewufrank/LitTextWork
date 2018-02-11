-----------------------------------------------------------------------------
--
-- Module      :  Language Typed Text
-- Copyright   :  andrew u frank -
--
-- | a text type which has as a type paramter the language


-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}

module Parser.LanguageTypedText
    (module Parser.LanguageTypedText
--    , module CoreNLP.Defs0
--    , module Parser.NLPvocabulary
    ) where

import           Test.Framework
--import Uniform.TestHarness (testVar3File)
import Uniform.Strings
import Data.RDF.Extension (LanguageCode (..))
--import CoreNLP.Defs0
--import Parser.TextDescriptor
--import NLP.Types.Tags
--import Parser.NLPvocabulary  -- from Foundation

data EnglishType
data GermanType
data FrenchType
data SpanishType
data ItalianType


newtype LCtext a = LCtext Text  deriving (Show, Eq, Read)
-- a piece of text in one language typed
unLCtext (LCtext text) = text

class LanguageTypedText lang where
    typeText :: lang -> Text -> LCtext lang
    typeText _ = LCtext


    sayLanguageOfText :: LCtext lang -> Text
    languageCode ::  lang -> LanguageCode

    -- just name the language
instance LanguageTypedText EnglishType where
    sayLanguageOfText _ = "English"
    languageCode _ = English

instance LanguageTypedText GermanType where
    sayLanguageOfText _ = "German"
    languageCode _ = German

instance LanguageTypedText FrenchType where
    sayLanguageOfText _ = "French"
    languageCode _ = French

instance LanguageTypedText SpanishType where
    sayLanguageOfText _ = "Spanish"
    languageCode _ = Spanish

instance LanguageTypedText ItalianType where
    sayLanguageOfText _ = "Italian"
    languageCode _ = Italian





