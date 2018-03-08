 -----------------------------------------------------------------------------
--
-- Module      :  Parser . ProduceDocCallNLP  --  main entry
-- Copyright   :  andrew u frank -
--
-- | convert a snip to  nlp triples

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

module NLP2RDF.ProduceDocCallNLP
    (module NLP2RDF.ProduceDocCallNLP
    , module LitTypes.ServerNames
    , module NLP2RDF.LanguageSpecific
    , LitTextFlags (..), LitTextFlag (..), SnipID (..),
    ) where

import CoreNLP.CoreNLPxml (readDocString)
import Uniform.HttpCall (callHTTP10post, addPort2URI, addToURI, URI, HttpVarParams(..))
import CoreNLP.ProduceNLPtriples -- (processDoc0toTriples2)
--
import NLP.Corpora.Conll  as Conll -- Conll for english
import NLP.Corpora.ItalianTinT   as TinT-- for italian
import NLP.Corpora.German  as German --
import NLP.Corpora.Spanish as Spanish --
import NLP.Corpora.French as French --
import NLP.Corpora.FrenchUD as FrenchUD --
import NLP.Types.Tags
----import Data.Text as T
import NLP2RDF.LanguageSpecific
import LitTypes.TextDescriptor  as TD
import LitTypes.ServerNames
import Data.RDFext.Codes

class  LanguageTyped22 lang postag where

    convertOneSnip2Triples2 :: lang -> postag -> LitTextFlags ->  Snip2 lang -> URI
                -> ErrIO [NLPtriple postag]
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

    snip2doc :: lang -> postag -> Bool ->  LTtext lang -> URI -> ErrIO (Doc0 postag)
    -- the nlp process, selected by language and postag

convertOneSnip2Triples3 :: LitTextFlags  ->   TD.Snip ->    ErrIO [Triple]
    -- this is  the entry point called from litText

convertOneSnip2Triples3 flags snip = do
    let lang = getLanguageCode . tz3text $  snip
    let snipsigl = tz3snipsigl snip   -- remove later
    let pt = ""  -- could be in flags
    let debugNLP = True --  DebugFlag `elem` flags
    let text = tz3text snip
    let nlpserver = if LocalNLPserverFlag `elem` flags then serverLocalhost else serverBrest
    trips <- case (lang, pt) of
        (English, "") -> do
                    t <- convertOneSnip2Triples2 undefEnglish undefConll
                                 flags  (Snip2 (convertLC2LT text) snipsigl) nlpserver
                    return (map unNLPtriple t)
        (German, "") -> do
                    t <- convertOneSnip2Triples2 undefGerman undefGermanPos
                                flags   (Snip2 (convertLC2LT text) snipsigl) nlpserver
                    return (map unNLPtriple t)
        (Italian,"") -> do
                    t <- convertOneSnip2Triples2 undefItalian undefTinTPos
                                flags   (Snip2 (convertLC2LT text) snipsigl) nlpserver
                    return (map unNLPtriple t)
        (French, "")-> do
                    t <-convertOneSnip2Triples2 undefFrench undefFrenchPos
                                flags   (Snip2 (convertLC2LT text) snipsigl) nlpserver
                    return (map unNLPtriple t)
        (French, "FrenchUD")-> do
                    t <- convertOneSnip2Triples2 undefFrench undefFrenchUDPos
                                flags   (Snip2 (convertLC2LT text) snipsigl) nlpserver
                    return (map unNLPtriple t)
        (Spanish,"") -> do
                    t <- convertOneSnip2Triples2 undefSpanish undefSpanishPos
                                flags   (Snip2 (convertLC2LT text) snipsigl) nlpserver
                    return (map unNLPtriple t)
        _ -> return zero
    return trips




instance (LanguageDependent lang, LanguageTypedText lang
        , TaggedTyped postag, POStags postag, LanguageTyped2 lang postag)
    =>  LanguageTyped22 lang postag where
    convertOneSnip2Triples2 lph pph flags snip sloc = do
        let debugNLP = DebugFlag `elem` flags
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
                let snipSigl = snip2sigl snip
                let trips = processDoc0toTriples2 lph pph snipSigl doc2

                return trips

    snip2doc lph pph debugNLP  text sloc = do
        let debug2 = debugNLP
        docs <-  convertTZ2makeNLPCall pph debug2
                            (addPort2URI sloc (nlpPort lph pph))  -- server uri
                            (nlpPath lph)   -- path
                                (nlpParams lph pph)  (unLCtext text)
        when debug2 $ putIOwords ["NLP end", showT text]
        return docs


--    trips2 <- if not . notNullLC $ text
--        then return zero
--        else do


class Docs postag where
    convertTZ2makeNLPCall  :: postag -> Bool -> URI -> Text -> HttpVarParams -> Text
                    ->  ErrIO (Doc0 postag)    -- the xml to analyzse  D -> E
    -- call to send text to nlp server and converts xml to Doc0
    -- works on individual paragraphs - but should treat bigger pieces if para is small (eg. dialog)
    -- merger
    -- the path parameter is used for TinT which wants a path "tint" before the paremters

instance (Docs2 postag, POStags postag) => Docs postag where
    convertTZ2makeNLPCall ph debugNLP  nlpServer path vars text = do
            xml :: Text <- text2xml ph debugNLP nlpServer path vars text
            xml2doc ph debugNLP   xml

class Docs2 postag where
    text2xml :: postag -> Bool -> URI -> Text -> HttpVarParams -> Text
                    ->  ErrIO Text
--                    ph debugNLP  nlpServer path vars text
    xml2doc :: postag -> Bool ->  Text ->  ErrIO (Doc0 postag)
--                    ph debugNLP   xml = do

instance (POStags postag) => Docs2 postag where
--    convertTZ2makeNLPCall  :: Bool ->  URI -> [(Text,Maybe Text)] -> Text ->  ErrIO (Doc0 postag)    -- the xml to analyzse  D -> E
    -- call to send text to nlp server and converts xml to Doc0
    -- works on individual paragraphs - but should treat bigger pieces if para is small (eg. dialog)
    -- merger


    text2xml ph debugNLP  nlpServer path vars text = do
            when debugNLP $
                putIOwords ["text2xml start"
                            , showT . lengthChar $ text
                            , showT . take' 100 $ text ]
            xml :: Text <- callHTTP10post debugNLP "multipart/form-data"  nlpServer path
                     (b2bl . t2b $ text) vars  (Just 300)   -- timeout in seconds
--            when debugNLP  $
            putIOwords ["text2xml end \n", take' 200 . showT    $  xml]
            return xml

        `catchError` (\e -> do
             putIOwords ["text2xml error caught 7",  e
                            ,  "\n\n the input was \n", text] -- " showT msg])
             putIOwords ["text2xml",  "text:\n",  showT text ] -- " showT msg])
             return zero
                )

    xml2doc ph debugNLP   xml = do
            when debugNLP $
                putIOwords ["xml2doc start"
                            , showT . take' 100 $ xml ]

            doc0 <- readDocString ph debugNLP xml                    -- E -> F
--            when debugNLP  $
            putIOwords ["xml2doc doc0 \n",  take' 200  . showT $ doc0]

            return   doc0
        `catchError` (\e -> do
             putIOwords ["xml2doc error caught 8",  e
--                            ,  "\n\n the input was \n", xml
                            ] -- " showT msg])
             putIOwords ["xml2doc" ] -- " showT msg])
    --         splitAndTryAgain debugNLP showXML nlpServer vars text
             return zero
                )


