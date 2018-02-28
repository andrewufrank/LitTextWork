 -----------------------------------------------------------------------------
--
-- Module      :  Parser . ProduceDocCallNLP  -- BAE=C -> D
-- Copyright   :  andrew u frank -
--
-- | convert the whole text to the selection which will be NLP analyzed

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

module Parser.ProduceDocCallNLP
    (module Parser.ProduceDocCallNLP
    , module Producer.Servers
    ) where

import              Test.Framework
import              Uniform.TestHarness
import Parser.LanguageTypedText
import Producer.Servers
import CoreNLP.CoreNLPxml (readDocString)
import CoreNLP.Defs0 ()
import Uniform.HttpCallWithConduit (callHTTP10post, addPort2URI, callHTTP8get, addToURI)
import Text.Regex (mkRegex, subRegex)
import Parser.CompleteSentence (completeSentence)
import Parser.ProduceNLPtriples -- (processDoc0toTriples2)
import Parser.LanguageTypedText

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


undefConll = undef "convertOneSnip2Triples postag conll":: Conll.POStag
undefGermanPos = undef "convertOneSnip2Triples postag german":: German.POStag
undefTinTPos = undef "convertOneSnip2Triples postat TinT":: TinT.POStag
undefFrenchPos = undef "convertOneSnip2Triples postat French":: French.POStag
undefFrenchUDPos = undef "convertOneSnip2Triples postat FrenchUD":: FrenchUD.POStag
undefSpanishPos = undef "convertOneSnip2Triples postat spanish":: Spanish.POStag

class LanguageDependent lang where

    preNLP :: LTtext lang -> LTtext lang
    -- the processing of the text before NLP
    preNLP = LTtext . cleanTextOther . unLCtext
    nlpPath :: lang -> Text
    nlpPath _ = ""   -- only italian uses a path

instance LanguageDependent EnglishType where
    preNLP    =  LTtext . cleanTextEnglish . unLCtext

instance LanguageDependent GermanType
instance LanguageDependent FrenchType
instance LanguageDependent SpanishType
instance LanguageDependent ItalianType where
    nlpPath _ = "tint"
    preNLP    =  LTtext . cleanTextItalian . unLCtext


class TaggedTyped postag where
    postNLP :: Bool -> URI -> Doc0 postag -> ErrIO (Doc0 postag)
    -- postprocessing (e.g. adding POS to german)
    postNLP _ _ = return

class ( POStags postag, LanguageDependent lang) =>  LanguageTyped2 lang postag where
    snip2doc :: lang -> postag -> Bool ->  LTtext lang -> URI -> ErrIO (Doc0 postag)
    -- the nlp process, selected by language and postag
    snip2doc lph pph debugNLP  text sloc = do
        let debug2 = debugNLP
        docs <-  convertTZ2makeNLPCall pph debug2
                            (addPort2URI sloc (nlpPort lph pph))  -- server uri
                            (nlpPath lph)   -- path
                                (nlpParams lph pph)  (unLCtext text)
        when debug2 $ putIOwords ["NLP end", showT text]
        return docs

    nlpParams :: lang -> postag -> [(Text,Maybe Text)]
    nlpPort :: lang -> postag -> Int   -- should be a port type

class  LanguageTyped22 lang postag where

    convertOneSnip2Triples2 :: lang -> postag -> Bool ->  Snip2 lang -> URI -> ErrIO [NLPtriple postag]
    -- this should be the entry point for conversion of a text to nlp
    -- typed in and output
    -- calls nlp to convert to doc
    -- the snip should have a type parameter language
    -- internal the text2nlp should have a tag type parameter
    -- the triples (i.e. NLPtriples should have a tag parameter

    -- convertOneSnip2Triples2 :: Bool -> Bool -> TextDescriptor -> LTtext lang -> ErrIO [NLPtriple postag]
    -- calls nlp to convert to doc
    -- the snip should have a type parameter language
    -- internal the text2nlp should have a tag type parameter
    -- the triples (i.e. NLPtriples should have a tag parameter

instance (LanguageDependent lang, LanguageTypedText lang, TaggedTyped postag, POStags postag, LanguageTyped2 lang postag)
    =>  LanguageTyped22 lang postag where
    convertOneSnip2Triples2 lph pph debugNLP snip sloc =
        if  snipIsNull snip
            then return []
            else do
                let text = snip2text snip
                when debugNLP $ putIOwords ["convertOneSnip2Triples" -- , sayLanguageOfText text
                                  , "\n text", showT text
                                  , "\n debug", showT debugNLP]
                let text2 = preNLP  text
--                let sloc = nlpServer textstate
                doc1 <- snip2doc lph pph debugNLP   text2 sloc

                doc2 <- postNLP debugNLP  sloc doc1

                let trips = processDoc0toTriples2 lph pph snip doc2

                return trips


instance TaggedTyped Conll.POStag
instance TaggedTyped German.POStag where
    postNLP debug sloc doc1  = do
        let sents1 = docSents doc1
        sents2 <- mapM (completeSentence False (addPort2URI sloc treeTaggerPort ) ) sents1
        let docs2 = doc1{docSents = sents2}
        return docs2

instance TaggedTyped TinT.POStag
instance TaggedTyped Spanish.POStag
instance TaggedTyped French.POStag
instance TaggedTyped FrenchUD.POStag

instance LanguageTyped2 EnglishType Conll.POStag where
    nlpPort _ _ = portEnglish
    nlpParams _ _ =   [("outputFormat", Just "xml")
                , ("annotators", Just "tokenize,ssplit,pos\
                                        \,lemma,ner,depparse,coref")]
            --                                    coref -coref.algorithm neural")

instance LanguageTyped2 GermanType German.POStag where
    nlpPort _ _ = portGerman
    nlpParams _ _ =   [("outputFormat", Just "xml"),
                        ("annotators", Just "tokenize,ssplit,pos,ner,depparse")
                                        ]

instance LanguageTyped2 ItalianType TinT.POStag where
    nlpPort _ _ = portTinT
    nlpParams _ _ =   [("format", Just "xml")]

--    nlpParams _ _ =   [("outputFormat", Just "xml"),
--                        ("annotators", Just "tokenize,ssplit,pos,ner,depparse")
--                                        ]

instance LanguageTyped2 FrenchType French.POStag where
    nlpPort _ _ = portFrench
    nlpParams _ _ =   [("outputFormat", Just "xml"),
                        ("annotators", Just "tokenize,ssplit,pos,lemma,ner,depparse,coref")
                                        ]

instance LanguageTyped2 FrenchType FrenchUD.POStag where
    nlpPort _ _ = portFrench
    nlpParams _ _ =   [("outputFormat", Just "xml"),
                        ("annotators", Just "tokenize,ssplit,pos,lemma,ner,depparse,coref")
                                        ]

instance LanguageTyped2 SpanishType Spanish.POStag where
    nlpPort _ _ = portSpanish
    nlpParams _ _ =   [("outputFormat", Just "xml"),
                        ("annotators", Just "tokenize,ssplit,pos,ner,depparse")
                                        ]

class Docs postag where
    convertTZ2makeNLPCall  :: postag -> Bool -> URI -> Text -> [(Text,Maybe Text)] -> Text ->  ErrIO (Doc0 postag)    -- the xml to analyzse  D -> E
    -- call to send text to nlp server and converts xml to Doc0
    -- works on individual paragraphs - but should treat bigger pieces if para is small (eg. dialog)
    -- merger
    -- the path parameter is used for TinT which wants a path "tint" before the paremters


instance (POStags postag) => Docs postag where
--    convertTZ2makeNLPCall  :: Bool ->  URI -> [(Text,Maybe Text)] -> Text ->  ErrIO (Doc0 postag)    -- the xml to analyzse  D -> E
    -- call to send text to nlp server and converts xml to Doc0
    -- works on individual paragraphs - but should treat bigger pieces if para is small (eg. dialog)
    -- merger
    convertTZ2makeNLPCall ph debugNLP  nlpServer path vars text = do
            when debugNLP $
                putIOwords ["convertTZ2makeNLPCall start"
                            , showT . lengthChar $ text
                            , showT . take' 100 $ text ]
    --        xml ::  Text  <-   makeHttpPost7 debugNLP nlpServer "" vars
    --                    "multipart/form-data" text
            xml :: Text <- callHTTP10post debugNLP "multipart/form-data"  nlpServer path
                     (b2bl . t2b $ text) vars  (Just 300)   -- timeout in seconds
        -- german parser seems to understand utf8encoded bytestring

--            when debugNLP  $
            putIOwords ["convertTZ2makeNLPCall end \n", take' 200 . showT    $  xml]

            doc0 <- readDocString ph True xml                    -- E -> F
                        -- bool controls production of XML output
--            when debugNLP  $
            putIOwords ["convertTZ2makeNLPCall doc0 \n",  take' 200  . showT $ doc0]

            return   doc0
        `catchError` (\e -> do
             putIOwords ["convertTZ2makeNLPCall error caught 7",  e
                            ,  "\n\n the input was \n", text] -- " showT msg])
             putIOwords ["convertTZ2makeNLPCall",  "text:\n",  showT text ] -- " showT msg])
    --         splitAndTryAgain debugNLP showXML nlpServer vars text
             return (Doc0 [] []) -- zero
                )

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


