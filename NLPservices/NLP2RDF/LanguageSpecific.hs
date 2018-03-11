 -----------------------------------------------------------------------------
--
-- Module      :  Parser . ProduceDocCallNLP  -- BAE=C -> D
-- Copyright   :  andrew u frank -
--
-- | convert snip to doc

-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables
    ,TypeSynonymInstances
    , MultiParamTypeClasses
    , NoMonomorphismRestriction
    , UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}

module NLP2RDF.LanguageSpecific
    (module NLP2RDF.LanguageSpecific
--    , module LitTypes.ServerNames
    ) where

import              Test.Framework
import              Uniform.TestHarness
import LitTypes.LanguageTypedText
import LitTypes.ServerNames

-- import CoreNLP.DocBase   -- should only get instances ?
import Uniform.HttpCall (URI, callHTTP10post, HttpVarParams(..))
import Text.Regex (mkRegex, subRegex)
import NLP2RDF.CompleteSentence (completeSentence)
import CoreNLP.Doc2ToLinear  -- for Doc1
import LitTypes.LanguageTypedText

import NLP.Corpora.Conll  as Conll -- Conll for english
import NLP.Corpora.ItalianTinT   as TinT-- for italian
import NLP.Corpora.German  as German --
import NLP.Corpora.Spanish as Spanish --
import NLP.Corpora.French as French --
import NLP.Corpora.FrenchUD as FrenchUD --

import Data.Text as T

portGerman = 9001 -- make port type
portEnglish = 9002
portFrench = 9003
portSpanish = 9004
portTinT = 9005
portFrenchUD = 9006

-- moved to individual POS tag files
--undefConll = undef "convertOneSnip2Triples postag conll":: Conll.POStag
--undefTinTPos = undef "convertOneSnip2Triples postat TinT":: TinT.POStag
--undefFrenchPos = undef "convertOneSnip2Triples postat French":: French.POStag
--undefFrenchUDPos = undef "convertOneSnip2Triples postat FrenchUD":: FrenchUD.POStag
--undefSpanishPos = undef "convertOneSnip2Triples postat spanish":: Spanish.POStag

class LanguageDependent lang where

    preNLP :: LTtext lang -> LTtext lang
    -- the processing of the text before NLP
    preNLP = LTtext . cleanTextOther . unLCtext
    nlpPath :: lang -> Text
    nlpPath _ = ""   -- only italian uses a path

class ( POStags postag, LanguageDependent lang) =>  LanguageTyped2 lang postag where
--    snip2doc :: lang -> postag -> Bool ->  LTtext lang -> URI -> ErrIO (Doc0 postag)
--    -- the nlp process, selected by language and postag
--    snip2doc lph pph debugNLP  text sloc = do
--        let debug2 = debugNLP
--        docs <-  convertTZ2makeNLPCall pph debug2
--                            (addPort2URI sloc (nlpPort lph pph))  -- server uri
--                            (nlpPath lph)   -- path
--                                (nlpParams lph pph)  (unLCtext text)
--        when debug2 $ putIOwords ["NLP end", showT text]
--        return docs

    nlpParams :: lang -> postag -> HttpVarParams
    nlpPort :: lang -> postag -> Int   -- should be a port type

instance LanguageDependent EnglishType where
    preNLP    =  LTtext . cleanTextEnglish . unLCtext

instance LanguageDependent GermanType
instance LanguageDependent FrenchType
instance LanguageDependent SpanishType
instance LanguageDependent ItalianType where
    nlpPath _ = "tint"
    preNLP    =  LTtext . cleanTextItalian . unLCtext


class TaggedTyped postag where
    postNLP :: Bool -> URI -> Doc1 postag -> ErrIO (Doc1 postag)
    -- postprocessing (e.g. adding POS to german)
    postNLP _ _ = return



instance TaggedTyped Conll.POStag
instance TaggedTyped German.POStag where
    postNLP debug sloc doc1  = do
        let sents1 = doc1Sents doc1
        sents2 <- mapM (completeSentence False
                    (addPort2URI sloc treeTaggerPort ) ) sents1
        let docs2 = doc1{doc1Sents = sents2}
        return docs2

instance TaggedTyped TinT.POStag
instance TaggedTyped Spanish.POStag
instance TaggedTyped French.POStag
instance TaggedTyped FrenchUD.POStag

instance LanguageTyped2 EnglishType Conll.POStag where
    nlpPort _ _ = portEnglish
    nlpParams _ _ =  HttpVarParams [
            -- only use json for english ?? TODO
                  ("annotators", Just "tokenize,ssplit,parse,pos\
                                        \,lemma,ner,depparse,coref")]
            --                                    coref -coref.algorithm neural")
--                  attention - no blanks between parameters!!!

instance LanguageTyped2 GermanType German.POStag where
    nlpPort _ _ = portGerman
    nlpParams _ _ =  HttpVarParams [
                        ("annotators", Just "tokenize,ssplit,pos,ner,depparse")
                                        ]

instance LanguageTyped2 ItalianType TinT.POStag where
    nlpPort _ _ = portTinT
    nlpParams _ _ =  HttpVarParams []

--    nlpParams _ _ =   [("outputFormat", Just "xml"),
--                        ("annotators", Just "tokenize,ssplit,pos,ner,depparse")
--                                        ]

instance LanguageTyped2 FrenchType French.POStag where
    nlpPort _ _ = portFrench
    nlpParams _ _ =  HttpVarParams [
                        ("annotators", Just "tokenize,ssplit,pos,lemma,ner,depparse,coref")
                                        ]

instance LanguageTyped2 FrenchType FrenchUD.POStag where
    nlpPort _ _ = portFrench
    nlpParams _ _ =  HttpVarParams [
                        ("annotators", Just "tokenize,ssplit,pos,lemma,ner,depparse,coref")
                                        ]

instance LanguageTyped2 SpanishType Spanish.POStag where
    nlpPort _ _ = portSpanish
    nlpParams _ _ =  HttpVarParams [
                        ("annotators", Just "tokenize,ssplit,pos,ner,depparse")
                                        ]


cleanTextEnglish :: Text -> Text
cleanTextEnglish    = subRegex' "_([a-zA-Z ]+)_" "\\1"  -- italics even multiple words
            . subRegex' "([0-9])([ds])."  "\\1 \\2 "   -- shiling/pence
            . subRegex' "([a-zA-Z]+)-([a-zA-Z]+)" "\\1 \\2"

cleanTextOther :: Text -> Text
cleanTextOther    = subRegex' "_([a-zA-Z ]+)_" "\\1"  -- italics even multiple words

cleanTextItalian :: Text -> Text
cleanTextItalian    = T.replace "ů" "u" . T.replace "ŕ" "a"     -- missing accents àèéìòóóù
                    . T.replace "č" "e" . T.replace "ň" "o"
                    . T.replace "ě" "i" . T.replace "Č" "e"    -- for perche
                    . T.replace "ę" "a"  -- ?? far provar, Danis??
                    . T.replace "ű" "u" . T.replace "ď" "i"  -- conclusion
                    . subRegex' "_([a-zA-Z ]+)_" "\\1"  -- italics even multiple words
-- Cosě  -- keine akzente gestzt, weil uneinheitlich im italienischen
-- to map would be ŕčňůěČęűď "\341\269\328\367\283\268\281\369\271"
{-
cleanTextGerman :: Text -> Text
cleanTextGerman    = subRegex' "_([a-zA-Z ]+)_" "\\1"  -- italics even multiple words

cleanTextFrench :: Text -> Text
cleanTextFrench    = subRegex' "_([a-zA-Z ]+)_" "\\1"  -- italics even multiple words

cleanTextspanish :: Text -> Text
cleanTextspanish    = subRegex' "_([a-zA-Z ]+)_" "\\1"  -- italics even multiple words

-}
subRegex' :: Text -> Text -> Text -> Text
-- replace the in the t the regex with the replacement
subRegex' reg rep t = s2t $ subRegex (mkRegex . t2s $ reg) (t2s t) (t2s rep)

