 -----------------------------------------------------------------------------
--
-- Module      :  Parser . ProduceDocCallNLP  -- BAE=C -> D
-- Copyright   :  andrew u frank -
--
-- | convert snip to doc

-----------------------------------------------------------------------------

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

module LitText.NLP2RDF.LanguageSpecific
    (module LitText.NLP2RDF.LanguageSpecific
    , module LitText.CoreNLP.CoreNLP
    , module LitText.Foundation
    ) where

import Uniform.Http
import Uniform.Strings
import Uniform.Error (ErrIO)
import LitText.CoreNLP.CoreNLP (conllu2NT, json2NT, NTtext (..), unNT
        , conllu2triples, json2triples)
import Text.Regex (mkRegex, subRegex)
import LitText.Foundation
import NLP.TagSets.Conll  as Conll -- Conll for english
import NLP.TagSets.ItalianTinT   as TinT-- for italian
import NLP.TagSets.German  as German --
import NLP.TagSets.Spanish as Spanish --
import NLP.TagSets.French as French --
import NLP.TagSets.FrenchUD as FrenchUD --
import NLP.TagSets.UD as UD --

import Data.Text as T

portGerman = mkPortNumber 9001 -- make port type
portEnglish = mkPortNumber 9002
portFrench = mkPortNumber 9003
portSpanish = mkPortNumber 9004
portTinT = mkPortNumber 9005
portFrenchUD = mkPortNumber 9006

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
    nlpPath :: lang -> HttpPath
    nlpPath _ = mkHttpPath ""   -- only italian uses a path

class ( POStags postag, LanguageDependent lang, LanguageTypedText lang)
        =>  LanguageTyped2 lang postag where

    nlpParams :: lang -> postag -> HttpQueryParams
    nlpPort :: lang -> postag -> PortNumber   -- should be a port type

    postNLP :: postag -> lang -> Bool -> RDFsubj -> Text -> [Triple]
    -- postprocessing (e.g. adding POS to german)
    postNLP pph lang debug  brdf txt =
            json2triples pph (languageCode lang) brdf txt

instance LanguageDependent EnglishType where
    preNLP    =  LTtext . cleanTextEnglish . unLCtext

instance LanguageDependent GermanType
instance LanguageDependent FrenchType
instance LanguageDependent SpanishType
instance LanguageDependent ItalianType where
    nlpPath _ = mkHttpPath "tint"
    preNLP    =  LTtext . cleanTextItalian . unLCtext




instance LanguageTyped2 EnglishType Conll.POStag where
    nlpPort _ _ = portEnglish
    nlpParams _ _ =  mkHttpQueryParams [("outputFormat", Just "json"),
            -- only use json for english ?? TODO
              ("annotators", Just "tokenize,ssplit,parse,pos\
                            \,lemma,ner,depparse,coref")]
--                   coref -coref.algorithm neural")
--                  attention - no blanks between parameters!!!

instance LanguageTyped2 EnglishType UD.POStag where
    nlpPort _ _ = portEnglish
    nlpParams _ _ =  mkHttpQueryParams
        [("outputFormat", Just "conllu"),
         ("annotators", Just
          "tokenize,ssplit,pos,lemma,ner,parse,dcoref,udfeats"
          )]
--        not working:
--               "tokenize,ssplit,parse,pos\
--                \,lemma,ner,depparse,coref,udfeats"
--                   udfeats needs constituency parser
    postNLP pph lang debug  brdf txt = conllu2triples brdf txt

instance LanguageTyped2 GermanType German.POStag where
    nlpPort _ _ = portGerman
    nlpParams _ _ =  mkHttpQueryParams [("outputFormat", Just "json"),
                        ("annotators", Just "tokenize,ssplit,pos,ner,depparse")
                                        ]

instance LanguageTyped2 ItalianType TinT.POStag where
    nlpPort _ _ = portTinT
    nlpParams _ _ =  mkHttpQueryParams []

--    nlpParams _ _ =   [("outputFormat", Just "xml"),
--                        ("annotators", Just "tokenize,ssplit,pos,ner,depparse")
--                                        ]

instance LanguageTyped2 FrenchType French.POStag where
    nlpPort _ _ = portFrench
    nlpParams _ _ =  mkHttpQueryParams [("outputFormat", Just "json"),
                        ("annotators", Just "tokenize,ssplit,pos,lemma,ner,depparse,coref")
                                        ]

instance LanguageTyped2 FrenchType FrenchUD.POStag where
    nlpPort _ _ = portFrench
    nlpParams _ _ =  mkHttpQueryParams [("outputFormat", Just "json"),
                        ("annotators", Just "tokenize,ssplit,pos,lemma,ner,depparse,coref")
                                        ]

--instance LanguageTyped2 SpanishType Spanish.POStag where
--    nlpPort _ _ = portSpanish
--    nlpParams _ _ =  mkHttpQueryParams [
--                        ("annotators", Just "tokenize,ssplit,pos,ner,depparse")
--                                        ]

instance LanguageTyped2 SpanishType Spanish.POStag where
    nlpPort _ _ = portSpanish
    nlpParams _ _ =  mkHttpQueryParams [("outputFormat", Just "json"),
                        ("annotators", Just "tokenize,ssplit,pos,ner,depparse")
                                        ]

instance LanguageTyped2 SpanishType UD.POStag where
    nlpPort _ _ = portSpanish
    nlpParams _ _ =  mkHttpQueryParams [("outputFormat", Just "json"),
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

