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
import Uniform.Zero (Zeros (..))
import Uniform.Strings
import Uniform.Error (undef)
import Data.RDF.Extension -- (LanguageCode (..))
--import CoreNLP.Defs0
--import Parser.TextDescriptor
--import NLP.Types.Tags
--import Parser.NLPvocabulary  -- from Foundation

data EnglishType
data GermanType
data FrenchType
data SpanishType
data ItalianType
data NoLanguageType

undefEnglish = undef "convertOneSnip2Triples lang engl" :: EnglishType
undefGerman = undef "convertOneSnip2Triples lang german" :: GermanType
undefItalian = undef "convertOneSnip2Triples lang ital":: ItalianType
undefFrench = undef "convertOneSnip2Triples lang french":: FrenchType
undefSpanish = undef "convertOneSnip2Triples lang spanish":: SpanishType
undefNoLanguage = undef "convertOneSnip2Triples no lang":: NoLanguageType


newtype LTtext a = LTtext Text  deriving (Show, Eq, Read)
-- a piece of text in one language typed
unLCtext (LTtext text) = text

class LanguageTypedText lang where
    typeText :: lang -> Text -> LTtext lang
    typeText _ = LTtext


    sayLanguageOfText :: LTtext lang -> Text
    languageCode ::  lang -> LanguageCode

    languageCode2undef :: LanguageCode -> lang

    convertLC2LT :: LCtext -> LTtext lang
    convertLC2LT (LCtext t lc) =
                typeText (languageCode2undef lc) t


    -- just name the language
instance LanguageTypedText EnglishType where
    sayLanguageOfText _ = "English"
    languageCode _ = English
    languageCode2undef English = undefEnglish

instance LanguageTypedText GermanType where
    sayLanguageOfText _ = "German"
    languageCode _ = German
    languageCode2undef German = undefGerman

instance LanguageTypedText FrenchType where
    sayLanguageOfText _ = "French"
    languageCode _ = French

instance LanguageTypedText SpanishType where
    sayLanguageOfText _ = "Spanish"
    languageCode _ = Spanish

instance LanguageTypedText ItalianType where
    sayLanguageOfText _ = "Italian"
    languageCode _ = Italian

instance LanguageTypedText NoLanguageType where
    sayLanguageOfText _ = "NoLanguage"
    languageCode _ = NoLanguage


data LCtext = LCtext {ltxt :: Text
                        , llang :: LanguageCode
                      } deriving (Read, Show, Eq, Ord)

class LanguageCodedText l where
    codeText  :: LanguageCode-> Text -> l
    getText :: l -> Text
    setLanguageCode :: LanguageCode -> l -> l
    getLanguageCode :: l -> LanguageCode
    getLengthLC :: l -> Int
    notNullLC :: l -> Bool
    sameLanguageLC :: l -> l -> Bool
    mergeLC :: Text -> l -> l -> Maybe l
    -- ^ merge with the separator between

instance LanguageCodedText LCtext where
    codeText lc t = LCtext t lc
    setLanguageCode lc2 (LCtext t lc) = LCtext t lc2
    getText = ltxt
    getLanguageCode = llang
    getLengthLC = lengthChar . getText
    notNullLC = (0 /=) . getLengthLC
    sameLanguageLC a b =  getLanguageCode a == getLanguageCode b
    mergeLC sep a b = if sameLanguageLC a b
        then Just $ a {ltxt = getText a <> sep <> getText b}
        else Nothing


mkTripleLang33 :: RDFsubj -> RDFproperty -> LCtext -> Triple
mkTripleLang33 o p lctext = mkTripleLang3 (getLanguageCode lctext) o p (getText lctext)


instance Zeros LCtext where
    zero = LCtext "" NoLanguage
