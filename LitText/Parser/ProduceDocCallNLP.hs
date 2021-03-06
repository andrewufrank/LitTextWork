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
    , UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}

module Parser.ProduceDocCallNLP
    (module Parser.ProduceDocCallNLP
    , module CoreNLP.Defs0
    , module Lines2para.Lines2para
    , module Producer.Servers
    , module Parser.FilterTextForNLP
    ) where

import           Test.Framework
import Uniform.TestHarness
--import Uniform.StringConversion (b2urlf)
import Data.Maybe -- todo
import Lines2para.Lines2para
import Lines2para.HandleLayout
import Parser.ReadMarkupAB  -- todo  -- for test
import Producer.Servers
import           CoreNLP.Defs0
import CoreNLP.CoreNLPxml (readDocString)
import Data.List.Split
import Uniform.HttpCallWithConduit (callHTTP10post, addPort2URI, callHTTP8get, addToURI)
import Text.Regex (mkRegex, subRegex)
import Parser.FilterTextForNLP
import Parser.CompleteSentence (completeSentence)
import Parser.ProduceNLPtriples (processDoc0toTriples2)
import NLP.Corpora.UD
import NLP.Corpora.Conll  as Conll -- Conll for english
import NLP.Corpora.ItalianTinT   -- for italian
import NLP.Corpora.German  -- for italian
import NLP.Corpora.Spanish -- for italian
import NLP.Corpora.French-- for italian
import NLP.Corpora.FrenchUD -- for italian


data EnglishType  -- should go with all the rest of language defs.
data GermanType
data FrenchType
data SpanishType
data ItalianType

portGerman = 9001
portEnglish = 9002
portFrench = 9003
portSpanish = 9004
portTinT = 9005
portFrenchUD = 9006

convertOneSnip2Triples :: Bool -> Bool -> TextDescriptor -> Snip -> ErrIO [Triple]
-- calls nlp to convert to doc
convertOneSnip2Triples debugNLP showXML textstate snip = do
        let text = tz3text snip
        -- reduce for some special cases _italics_
        -- should be done before the split, because . of abbreviations are elliminated
        if null' text
            then return zero
            else do
                let language = tz3lang snip
--                let sloc = nlpServer textstate
                when debugNLP $ putIOwords ["convertOneSnip2Triples", "language"
                            , showT language, "\n snip", showT snip]

                case language of
                    English -> snip2triples2 (undef "convertOneSnip2Triples lang engl" :: EnglishType)
                                            (undef "convertOneSnip2Triples postat":: Conll.POStag )
                                            debugNLP showXML textstate snip
                    German -> snip2triples2 (undef "convertOneSnip2Triples lang engl" :: GermanType)
                                            (undef "convertOneSnip2Triples postat":: POStagGerman)
                                            debugNLP showXML textstate snip
                    Italian -> snip2triples2 (undef "convertOneSnip2Triples lang ital":: ItalianType)
                                            (undef "convertOneSnip2Triples postat":: POStagTinT)
                                            debugNLP showXML textstate snip
                    French -> snip2triples2 (undef "convertOneSnip2Triples lang ital":: FrenchType)
                                            (undef "convertOneSnip2Triples postat":: POStagFrench)
--                                            (undef "convertOneSnip2Triples postat":: POStagFrenchUD)
                                            debugNLP showXML textstate snip
                    Spanish -> snip2triples2 (undef "convertOneSnip2Triples lang ital":: SpanishType)
                                            (undef "convertOneSnip2Triples postat":: POStagSpanish)
                                            debugNLP showXML textstate snip
                --                    NoLanguage -> return zero
                    _    -> do
                            putIOwords ["convertOneSnip2Triples", "no server for ", showT language]
                            return []
--                    _    -> errorT ["convertOneSnip2Triples"
--                                            , showT language, "language has no server"]
testOP_DA_L :: TextDescriptor -> [Snip]-> ErrIO [[Triple]]
testOP_DA_L textstate = mapM (convertOneSnip2Triples True True textstate)


class Docs postag where
    convertTZ2makeNLPCall  :: postag -> Bool -> Bool -> URI -> [(Text,Maybe Text)] -> Text ->  ErrIO (Doc0 postag)    -- the xml to analyzse  D -> E
    -- call to send text to nlp server and converts xml to Doc0
    -- works on individual paragraphs - but should treat bigger pieces if para is small (eg. dialog)
    -- merger

-- (CharChains2 postag Text) =>
class (Show postag) =>  LanguageSpecificNLPcall  langPhantom postag where
    snip2triples2 :: langPhantom -> postag -> Bool -> Bool -> TextDescriptor -> Snip -> ErrIO [Triple] -- (Doc0 postag)
    -- process an english text snip to a Doc0

instance (POStags postag) => Docs postag where


--    convertTZ2makeNLPCall  :: Bool -> Bool -> URI -> [(Text,Maybe Text)] -> Text ->  ErrIO (Doc0 postag)    -- the xml to analyzse  D -> E
    -- call to send text to nlp server and converts xml to Doc0
    -- works on individual paragraphs - but should treat bigger pieces if para is small (eg. dialog)
    -- merger
    convertTZ2makeNLPCall ph debugNLP showXML nlpServer vars text = do
            when debugNLP $
                putIOwords ["convertTZ2makeNLPCall start"
                            , showT . lengthChar $ text
                            , showT . take' 100 $ text ]
    --        xml ::  Text  <-   makeHttpPost7 debugNLP nlpServer "" vars
    --                    "multipart/form-data" text
            xml :: Text <- callHTTP10post debugNLP "multipart/form-data"  nlpServer ""
                     (b2bl . t2b $ text) vars  (Just 300)   -- timeout in seconds
        -- german parser seems to understand utf8encoded bytestring

            when debugNLP  $
                putIOwords ["convertTZ2makeNLPCall end \n", showT xml]

            doc0 <- readDocString ph showXML xml                    -- E -> F
            when debugNLP  $
                putIOwords ["convertTZ2makeNLPCall doc0 \n", showT doc0]

            return   doc0
        `catchError` (\e -> do
             putIOwords ["convertTZ2makeNLPCall http error caught 7",  e] -- " showT msg])
             putIOwords ["convertTZ2makeNLPCall",  "text:\n",  showT text ] -- " showT msg])
    --         splitAndTryAgain debugNLP showXML nlpServer vars text
             return zero
                )

instance LanguageSpecificNLPcall EnglishType Conll.POStag where
--    englishNLP :: Bool -> Bool -> URI -> Text -> ErrIO Doc0
    -- process an english text snip to a Doc0
--    englishNLP debugNLP showXML sloc text = do
    snip2triples2 _ tagPhantom debugNLP showXML textstate snip = do
        let varsEng =  [("outputFormat", Just "xml")
                , ("annotators", Just "tokenize,ssplit,pos\
                                        \,lemma,ner,depparse, dcoref,coref")
            --                                    coref -coref.algorithm neural")
    --        -- perhaps the nerual algorithm is better, but creates problems
    --        -- with the xml doc received (starts with C?
                                        ]
        when debugNLP $ putIOwords ["englishNLP text", showT  $ tz3text snip]

        let text2 = cleanTextEnglish $  tz3text snip
        let sloc = nlpServer textstate
        docs <-  convertTZ2makeNLPCall tagPhantom debugNLP showXML (addPort2URI sloc portEnglish) varsEng  text2
    --            when False $ putIOwords ["englishNLP parse"
    --                    , sparse . headNote "docSents" . docSents . headNote "xx243" $ docs]
        when debugNLP $ putIOwords ["englishNLP end", showT text2]
--        let docs2 = docs `asTypeOf` doc0Phantom
        let snipnr = 1 -- TODO

        let trips = processDoc0toTriples2 textstate (tz3lang snip) (tz3para $ snip) (snipnr, (docs))

        return trips



instance LanguageSpecificNLPcall GermanType POStagGerman where
--    englishNLP :: Bool -> Bool -> URI -> Text -> ErrIO Doc0
    -- process an english text snip to a Doc0
--    englishNLP debugNLP showXML sloc text = do
    snip2triples2 _ tagPhantom debugNLP showXML textstate snip = do
--    germanNLP :: Bool -> Bool -> URI -> Text -> ErrIO Doc0
    -- process an german text snip to a Doc0
--    germanNLP debugNLP showXML sloc text = do
        let vars =  [("outputFormat", Just "xml"),
                        ("annotators", Just "tokenize,ssplit,pos,ner,depparse")
                                        ]
        when debugNLP $ putIOwords ["germanNLP text", showT  $ tz3text snip]

        let text2 = cleanTextGerman $  tz3text snip
        let sloc = nlpServer textstate


        docs <-  convertTZ2makeNLPCall tagPhantom debugNLP showXML (addPort2URI sloc portGerman ) vars  text2
        when debugNLP $ putIOwords ["englishNLP end", showT text2]
--        let docs2 = docs `asTypeOf` doc0Phantom
        let sents1 = docSents docs
        sents2 <- mapM (completeSentence False (addPort2URI sloc 17701 ) ) sents1
        let docs2 = docs{docSents = sents2}

        let snipnr = 1 -- TODO

        let trips = processDoc0toTriples2 textstate (tz3lang snip) (tz3para $ snip) (snipnr, docs2)

        return trips


--
instance LanguageSpecificNLPcall FrenchType POStagFrench where
--    englishNLP :: Bool -> Bool -> URI -> Text -> ErrIO Doc0
    -- process an english text snip to a Doc0
--    englishNLP debugNLP showXML sloc text = do
    snip2triples2 _ tagPhantom debugNLP showXML textstate snip = do

--    frenchNLP :: Bool -> Bool -> URI -> Text -> ErrIO Doc0
    -- process an french text snip to a Doc0
--    frenchNLP debugNLP showXML sloc text = do
        let vars  =  [("outputFormat", Just "xml")
                , ("annotators", Just "tokenize,ssplit,pos,lemma,ner,depparse,coref")
    --        --                                    coref -coref.algorithm neural")
    --        -- perhaps the nerual algorithm is better, but creates problems
    --        -- with the xml doc received (starts with C?
    --        --                                        dcoref,coref")
    --                --                    --  coref, verlangt depparse,
    --                                    ,

                                        ]
--        when debugNLP $
        putIOwords ["frenchNLP text", showT  $ tz3text snip]

        let text2 = cleanTextFrench  $  tz3text snip
        let sloc = nlpServer textstate

    --            let texts = getPiece . textSplit $ text2
    --            let texts = if True -- lengthChar text2 < nlpDocSizeLimit
    --                            then [ text2]
    --                            else getPiece nlpDocSizeLimit . textSplit2 $ text2

        docs <-  convertTZ2makeNLPCall tagPhantom debugNLP showXML (addPort2URI sloc portFrench ) vars  text2
        when debugNLP $ putIOwords ["french end", showT text2]
--        let docs2 = docs `asTypeOf` doc0Phantom
        let snipnr = 1 -- TODO

        let trips = processDoc0toTriples2 textstate (tz3lang snip) (tz3para $ snip) (snipnr, (docs))

        return trips
--
instance LanguageSpecificNLPcall FrenchType POStagFrenchUD where
---- the tagset is perhaps incomplete and does not produce corefs. the normal model is ok
--
    snip2triples2 _ tagPhantom debugNLP showXML textstate snip = do
        let vars  =  [("outputFormat", Just "xml")
                , ("annotators", Just "tokenize,ssplit,pos,lemma,ner,depparse,coref")
                                        ]
--        when debugNLP $
        putIOwords ["frenchNLP text", showT  $ tz3text snip]

        let text2 = cleanTextFrench  $  tz3text snip
        let sloc = nlpServer textstate

        docs <-  convertTZ2makeNLPCall tagPhantom debugNLP showXML (addPort2URI sloc portFrenchUD ) vars  text2
        when debugNLP $ putIOwords ["french UD end", showT text2]
        let snipnr = 1 -- TODO

        let trips = processDoc0toTriples2 textstate (tz3lang snip) (tz3para $ snip) (snipnr, docs)

        return trips


instance LanguageSpecificNLPcall SpanishType POStagSpanish where
--    englishNLP :: Bool -> Bool -> URI -> Text -> ErrIO Doc0
    -- process an english text snip to a Doc0
--    englishNLP debugNLP showXML sloc text = do
    snip2triples2 _ tagPhantom debugNLP showXML textstate snip = do
--    spanishNLP :: Bool -> Bool -> URI -> Text -> ErrIO Doc0
    -- process an spanish text snip to a Doc0
--    spanishNLP debugNLP showXML sloc text = do
        let vars =  [("outputFormat", Just "xml")
                , ("annotators", Just "tokenize,ssplit,pos,lemma,ner,depparse,coref")
    --        --                                    coref -coref.algorithm neural")
    --        -- perhaps the nerual algorithm is better, but creates problems
    --        -- with the xml doc received (starts with C?
    --        --                                        dcoref,coref")
    --                --                    --  coref, verlangt depparse,
    --                                    ,
    --                                    ("outputFormat", Just "xml")
                                        ]
        when debugNLP $ putIOwords ["spanishNLP text", showT  $ tz3text snip]

        let text2 = cleanTextspanish  $  tz3text snip
        let sloc = nlpServer textstate

    --            let texts = getPiece . textSplit $ text2
    --            let texts = if True -- lengthChar text2 < nlpDocSizeLimit
    --                            then [ text2]
    --                            else getPiece nlpDocSizeLimit . textSplit2 $ text2

        docs <-  convertTZ2makeNLPCall tagPhantom debugNLP showXML (addPort2URI sloc portSpanish ) vars  text2
        when debugNLP $ putIOwords ["englishNLP end", showT text2]
--        let docs2 = docs `asTypeOf` doc0Phantom
        let snipnr = 1 -- TODO

        let trips = processDoc0toTriples2 textstate (tz3lang snip) (tz3para $ snip) (snipnr, (docs))

        return trips


instance LanguageSpecificNLPcall ItalianType POStagTinT where
    snip2triples2 _ tagPhantom debugNLP showXML textstate snip = do
--    italianNLP :: Bool -> Bool -> URI -> Text -> ErrIO Doc0
    -- process an italian text snip to a Doc0
--    italianNLP debugNLP showXML sloc text = do
            let vars =  [
                                            ]

            let text2 = cleanTextitalian $  tz3text snip
            let sloc = nlpServer textstate

            when debugNLP $ putIOwords ["italianNLP text2", text2]

            xml :: Text <- callHTTP10post debugNLP  "multipart/form-data"  (addPort2URI sloc portTinT) "tint?format=xml"
                         (b2bl . t2b $ text2) vars  (Just 300)   -- timeout in seconds

            when debugNLP $ putIOwords ["italianNLP xml",  xml]

            docs <- readDocString tagPhantom showXML xml
            when debugNLP $ putIOwords ["italianNLP doc", showT docs]
--
            let snipnr = 1 -- TODO
            let trips = processDoc0toTriples2 textstate (tz3lang snip) (tz3para $ snip) (snipnr, (docs))
            when debugNLP $ putIOwords ["italianNLP end", showT trips]

            return trips
        `catchError` (\e -> do
             putIOwords ["italianNLP   error caught  ",  e] -- " showT msg])
             putIOwords ["italianNLP",  "text:\n",  showT $ tz3text snip ] -- " showT msg])
    --         splitAndTryAgain debugNLP showXML nlpServer vars text
             return zero
                )


cleanTextEnglish :: Text -> Text
cleanTextEnglish    = subRegex' "_([a-zA-Z ]+)_" "\\1"  -- italics even multiple words
            . subRegex' "([0-9])([ds])."  "\\1 \\2 "   -- shiling/pence
            . subRegex' "([a-zA-Z]+)-([a-zA-Z]+)" "\\1 \\2"

cleanTextGerman :: Text -> Text
cleanTextGerman    = subRegex' "_([a-zA-Z ]+)_" "\\1"  -- italics even multiple words

cleanTextFrench :: Text -> Text
cleanTextFrench    = subRegex' "_([a-zA-Z ]+)_" "\\1"  -- italics even multiple words

cleanTextspanish :: Text -> Text
cleanTextspanish    = subRegex' "_([a-zA-Z ]+)_" "\\1"  -- italics even multiple words

cleanTextitalian :: Text -> Text
cleanTextitalian    = subRegex' "_([a-zA-Z ]+)_" "\\1"  -- italics even multiple words



--test_1_DA_L = testVar3FileIO result1A "resultDA1" "resultE1" testOP_DA_L
--test_2_DA_L = testVar3FileIO result2A "resultDA2" "resultE2" testOP_DA_L
--test_3_DA_L = testVar3FileIO result3A "resultDA3" "resultE3" testOP_DA_L
--test_4_DA_L = testVar3FileIO result4A "resultDA4" "resultE4" testOP_DA_L
--test_5_DA_L = testVar3FileIO result5A "resultDA5" "resultE5" testOP_DA_L  -- lafayette
--test_6_DA_L = testVar3FileIO result6A "resultDA6" "resultE6" testOP_DA_L
--test_8_DA_L = testVar3FileIO result8A "resultDA8" "resultE8" testOP_DA_L
--test_9_DA_L = testVar3FileIO result9A "resultDA9" "resultE9" testOP_DA_L
--test_10_DA_L = testVar3FileIO result10A "resultDA10" "resultE10" testOP_DA_L
--test_11_DA_L = testVar3FileIO result11A "resultDA11" "resultE11" testOP_DA_L
test_12_DA_L = testVar3FileIO result12A "resultDA12" "resultE12" testOP_DA_L
--test_13_DA_L = testVar3FileIO result12A "resultDA12" "resultE12UD" testOP_DA_L


-- the result goes to /home/frank/Scratch/NT/LitTest/test (defined in foundation as testNTdir
-- setup in readMarkup

--------------------------


subRegex' :: Text -> Text -> Text -> Text
-- replace the in the t the regex with the replacement
subRegex' reg rep t = s2t $ subRegex (mkRegex . t2s $ reg) (t2s t) (t2s rep)

--test_clean1 = assertEqual "The father went to the row house for 2 d  and got it."
--        (cleanTextEnglish "The _father_ went to the row-house for 2d. and got it.")
--test_clean2 = assertEqual "  Knots of idle men  on the South Bridge, for 3 s  2 d  .\
--        \   This street named the Via Dolorosa."
-- (cleanTextEnglish tx1)
--
--tx1 = "  Knots of idle-men  \
--    \on the South Bridge, for 3s. 2d. .   \
--    \This street named the _Via Dolorosa_."

