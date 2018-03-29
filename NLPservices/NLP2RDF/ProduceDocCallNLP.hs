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
    , RecordWildCards
    , UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}

module NLP2RDF.ProduceDocCallNLP
    (module NLP2RDF.ProduceDocCallNLP
    , module LitTypes.ServerNames
    , module NLP2RDF.LanguageSpecific
    , LitTextFlags (..), LitTextFlag (..), SnipID (..)
    ) where

--import CoreNLP.CoreNLPxml (readDocString)
import CoreNLP.ParseJsonCoreNLP (decodeDoc2, Doc2 (..))
import Uniform.HttpCall (callHTTP10post, addPort2URI, addToURI
            , URI, HttpVarParams(..), combineHttpVarParams)
-- version for xml (old)
--import CoreNLP.ProduceNLPtriples -- (processDoc0toTriples2)

-- version based on JSON parsing :
--import CoreNLP.ProduceNLPtriples2 -- (processDoc0toTriples2)
--
-- version with Doc2ToRDF_JSON
import CoreNLP.CoreNLP -- Doc2ToLinear

--import NLP2RDF.ProduceNLPtriples (Snip2(..))

import NLP.TagSets.Conll  as Conll -- Conll for english
import NLP.TagSets.ItalianTinT   as TinT-- for italian
import NLP.TagSets.German  as German --
import NLP.TagSets.Spanish as Spanish --
import NLP.TagSets.French as French --
import NLP.TagSets.FrenchUD as FrenchUD --
import NLP.Tags
----import Data.Text as T
import NLP2RDF.LanguageSpecific
import LitTypes.TextDescriptor  as TD
import LitTypes.ServerNames
import Data.RDFext.Codes
import Uniform.Zero
import LitTypes.ServerNames (rdfBase)  -- replace!!

import Data.ByteString.Lazy (fromStrict)  -- move to decode

convertOneSnip2triples_NLPservices :: LitTextFlags  -> TD.Snip -> ErrIO [Triple]
-- | is is the overall process taking a snip and producing the NT as text
-- construct a snipID from rdfBase (from LitTypes.ServerNames)
-- this is just the conversion from tagged to typed
-- which could not be moved earlier, because the snip is the highest
-- single language text collection
-- the tagset is selected in the snip, unless "" which  means default
-- then selection on languageCode
-- not used: LitTextFlag (contains the posTagSet, but this is used
-- in LitText
convertOneSnip2triples_NLPservices flags Snip{..} =
  if  snip3textLength == 0
    then return zero
    else do
        let lang = getLanguageCode  snip3text
    --    let snipsigl = snip3snipsigl snip   -- remove later, something is needed
        let snipBase = snip3baserdf
        let postagsetID =  snip3posTagSetID
        let debugNLP = True --  DebugFlag `elem` flags
        let text = snip3text
        let nlpserver = if LocalNLPserverFlag `elem` flags
                    then serverLocalhost
                    else serverBrest
        trips <- case (lang, postagsetID) of
            (English, "") -> do
                    t <- snip2triples undefEnglish Conll.undefPOS
                         flags   (convertLC2LT text) snipBase  nlpserver
                    return t -- (map unNLPtriple t)
            (German, "") -> do
                    t <- snip2triples undefGerman German.undefPOS
                         flags   (convertLC2LT text) snipBase  nlpserver
                    return t -- (map unNLPtriple t)
            (Italian,"") -> do
                    t <- snip2triples undefItalian TinT.undefPOS
                         flags   (convertLC2LT text) snipBase  nlpserver
                    return t -- (map unNLPtriple t)
            (French, "")-> do
                    t <-snip2triples undefFrench French.undefPOS
                         flags   (convertLC2LT text) snipBase  nlpserver
                    return t -- (map unNLPtriple t)
            (French, "FrenchUD")-> do
                    t <- snip2triples undefFrench FrenchUD.undefPOS
                         flags   (convertLC2LT text) snipBase  nlpserver
                    return t -- (map unNLPtriple t)
            (Spanish,"") -> do
                    t <- snip2triples undefSpanish Spanish.undefPOS
                         flags   (convertLC2LT text) snipBase  nlpserver
                    return t -- (map unNLPtriple t)
            _ -> return zero
        return trips


class  LanguageTyped22 lang postag where

--    convertOneSnip2Triples2 :: lang -> postag -> LitTextFlags ->  Snip2 lang -> URI
--                -> ErrIO [NLPtriple postag]
    snip2triples :: lang -> postag -> LitTextFlags ->  LTtext lang
                -> RDFsubj  -> URI
                -> ErrIO [Triple]
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

--    snip2doc :: lang -> postag -> Bool ->  LTtext lang -> URI -> ErrIO (Doc1 postag)
    snip2doc :: lang -> postag -> Bool ->  LTtext lang -> URI
                -> ErrIO Text
    -- the nlp process, selected by language and postag
    -- results in a text as json



instance (-- LanguageDependent lang,
        LanguageTypedText lang
--        , TaggedTyped postag
        , POStags postag
        , LanguageTyped2 lang postag
        )
    =>
    LanguageTyped22 lang postag where
    snip2triples lph pph flags lttext baserdf sloc = do
        let debugNLP = DebugFlag `elem` flags

        let text = lttext -- snip2text snip
        when debugNLP $ putIOwords ["convertOneSnip2Triples" -- , sayLanguageOfText text
                          , "\n text", showT text
                          , "\n debug", showT debugNLP]
        let text2 = text -- preNLP  text
--                let sloc = nlpServer textstate
        doc1 <- snip2doc lph pph debugNLP   text2  sloc
            -- doc1 is the nlp produced document (xml, json or conllu)
        let doc2 = postNLP pph lph debugNLP baserdf doc1
--                let snipSigl = snip2sigl snip
--                let trips = processDoc1toTriples2 lph pph snipSigl doc2
--        let nts = json2NT (baserdf) doc2
        -- here is the difference between languages
        -- make this a class selected by lang and postag and perhaps NERtag
        -- move all processing after the call to nlp to
        -- postNLP : doc1 -> NT text
        return doc2

    snip2doc lph pph debugNLP  text  sloc = do
        let debug2 = debugNLP
        code1 <-  text2nlpCode  debug2
                            (addPort2URI sloc (nlpPort lph pph))  -- server uri
                            (nlpPath lph)   -- path
                                (nlpParams lph pph)  (unLCtext text)
--        docs <- nlpCode2doc1 pph debugNLP code1
        when debug2 $ putIOwords ["NLP end", showT code1]
        return code1


--class Docs2 postag where
text2nlpCode :: Bool -> URI -> Text -> HttpVarParams -> Text
                    ->  ErrIO Text
--                    ph debugNLP  nlpServer path vars text
--    nlpCode2doc1 :: postag -> Bool ->  Text ->  ErrIO (Doc1 postag)
----                    ph debugNLP   nlpCode = do

--instance (POStags postag) => Docs2 postag where
--    text2nlpcode  :: Bool ->  URI -> [(Text,Maybe Text)] -> Text ->  ErrIO (Doc0 postag)    -- the nlpCode to analyzse  D -> E
    -- call to send text to nlp server and converts nlpCode to Doc0
    -- works on individual paragraphs - but should treat bigger pieces if para is small (eg. dialog)
    -- merger


text2nlpCode debugNLP  nlpServer path vars text = do
            when debugNLP $
                putIOwords ["text2nlpCode start"
                            , showT . lengthChar $ text
                            , showT . take' 100 $ text ]
--            let vars2 = combineHttpVarParams vars
--                    (HttpVarParams [("outputFormat", Just "json")])
            -- alternative ("outputFormat", Just "xml"),
            -- or conllu
            nlpCode :: Text <- callHTTP10post debugNLP
                            "multipart/form-data"  nlpServer path
                            (b2bl . t2b $ text) vars
                            (Just 300)   -- timeout in seconds
--            when debugNLP  $

            putIOwords ["text2nlpCode end \n", take' 200 . showT    $  nlpCode]
            return nlpCode

        `catchError` (\e -> do
             putIOwords ["text2nlpCode error caught 7 -- nlp service issue ",  e
                            ,  "\n\n the input was \n", text] -- " showT msg])
             putIOwords ["text2nlpCode",  "text:\n",  showT text ] -- " showT msg])
             return zero
                )

--    nlpCode2doc1 ph debugNLP   nlpCode = do
--            when debugNLP $
--                putIOwords ["nlpCode2doc start"
--                            , showT . take' 100 $ nlpCode ]
--
----            doc0 <- readDocString ph debugNLP nlpCode                    -- E -> F
--            let doc2e = decodeDoc2 . fromStrict . t2b $ nlpCode
--            doc2 <- either (throwError  . s2t) (return ) doc2e     -- :: Doc2                                     -- E -> F
--            let doc1 = doc2to1 ph doc2
----            when debugNLP  $
--            putIOwords ["nlpCode2doc doc0 \n",  take' 200  . showT $ doc1]
--
--            return   doc1
--        `catchError` (\e -> do
--             putIOwords ["nlpCode2doc error caught 8 - parse issue ",  e
----                            ,  "\n\n the input was \n", nlpCode
--                            ] -- " showT msg])
--             putIOwords ["nlpCode2doc" ] -- " showT msg])
--    --         splitAndTryAgain debugNLP showXML nlpServer vars text
--             return zero
--                )


--convertOneSnip2Triples3 :: LitTextFlags  -> TD.Snip -> ErrIO [Triple]
--    -- this is  the entry point called from litText
--
--convertOneSnip2Triples3 flags snip = do
--    let lang = getLanguageCode . snip3text $  snip
--    let snipsigl = snip3snipsigl snip   -- remove later
--    let pt = ""  -- could be in flags
--    let debugNLP = True --  DebugFlag `elem` flags
--    let text = snip3text snip
--    let nlpserver = if LocalNLPserverFlag `elem` flags then serverLocalhost else serverBrest
--    trips <- case (lang, pt) of
--        (English, "") -> do
--                    t <- convertOneSnip2Triples2 undefEnglish undefConll
--                                 flags  (Snip2 (convertLC2LT text) snipsigl) nlpserver
--                    return (map unNLPtriple t)
--        (German, "") -> do
--                    t <- convertOneSnip2Triples2 undefGerman undefGermanPos
--                                flags   (Snip2 (convertLC2LT text) snipsigl) nlpserver
--                    return (map unNLPtriple t)
--        (Italian,"") -> do
--                    t <- convertOneSnip2Triples2 undefItalian undefTinTPos
--                                flags   (Snip2 (convertLC2LT text) snipsigl) nlpserver
--                    return (map unNLPtriple t)
--        (French, "")-> do
--                    t <-convertOneSnip2Triples2 undefFrench undefFrenchPos
--                                flags   (Snip2 (convertLC2LT text) snipsigl) nlpserver
--                    return (map unNLPtriple t)
--        (French, "FrenchUD")-> do
--                    t <- convertOneSnip2Triples2 undefFrench undefFrenchUDPos
--                                flags   (Snip2 (convertLC2LT text) snipsigl) nlpserver
--                    return (map unNLPtriple t)
--        (Spanish,"") -> do
--                    t <- convertOneSnip2Triples2 undefSpanish undefSpanishPos
--                                flags   (Snip2 (convertLC2LT text) snipsigl) nlpserver
--                    return (map unNLPtriple t)
--        _ -> return zero
--    return trips
