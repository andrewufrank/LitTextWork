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
{-# LANGUAGE ScopedTypeVariables #-}
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
import Data.Maybe -- todo
import Lines2para.Lines2para
import Lines2para.HandleLayout
import Parser.ReadMarkupAB  -- todo  -- for test
import Producer.Servers
import           CoreNLP.Defs0
import CoreNLP.CoreNLPxml (readDocString)
import Data.List.Split
import Uniform.HttpCallWithConduit (callHTTP10post, addPort2URI)
import Text.Regex (mkRegex, subRegex)
import Parser.FilterTextForNLP
import Parser.CompleteSentence (completeSentence)

snip2doc :: Bool -> Bool -> URI -> Snip -> ErrIO   Doc0     -- the xml to analyzse  D -> E
---- ^ the entry point for conversionm of one snip (piece of text) to Doc0 in the xml format of stanford CoreNLP
snip2doc debugNLP showXML sloc snip = do
    when debugNLP $ putIOwords ["snip2doc start"]
    when False $ putIOwords ["snip2doc snip", showT snip]
    convertTZ2nlpPrepareCall debugNLP showXML sloc snip
--

--
--
---- | tests which would call NLP
testOP_C_E :: TextDescriptor -> [Snip]-> ErrIO [Doc0]  --   [(NLPtext,[Doc0])]
testOP_C_E resultXA resultDAfile = do
    let sloc = nlpServer  resultXA

    res <-  mapM (snip2doc False   False sloc) resultDAfile
    -- the secnd bool controls the rendering of the xml file
    putIOwords ["testOP_C_E",  "result:\n",  showT res ] -- " showT msg])
    -- leave in to force processing and continous output
    return res
--
test_1_C_E = testVar3FileIO result1A "resultDA1" "resultE1" testOP_C_E
test_2_C_E = testVar3FileIO result2A "resultDA2" "resultE2" testOP_C_E
test_3_C_E = testVar3FileIO result3A "resultDA3" "resultE3" testOP_C_E
test_4_C_E = testVar3FileIO result4A "resultDA4" "resultE4" testOP_C_E
test_5_C_E = testVar3FileIO result5A "resultDA5" "resultE5" testOP_C_E  -- lafayette
test_6_C_E = testVar3FileIO result6A "resultDA6" "resultE6" testOP_C_E
test_8_C_E = testVar3FileIO result8A "resultDA8" "resultE8" testOP_C_E
--test_9_C_E = testVar3FileIO result9A "resultBAE9" "resultE9" testOP_C_E
--test_10_C_E = testVar3FileIO result10A "resultBAE10" "resultE10" testOP_C_E

--------------------------


subRegex' :: Text -> Text -> Text -> Text
-- replace the in the t the regex with the replacement
subRegex' reg rep t = s2t $ subRegex (mkRegex . t2s $ reg) (t2s t) (t2s rep)

test_clean1 = assertEqual "The father went to the row house for 2 d  and got it."
        (cleanTextEnglish "The _father_ went to the row-house for 2d. and got it.")
test_clean2 = assertEqual "  Knots of idle men  on the South Bridge, for 3 s  2 d  .\
        \   This street named the Via Dolorosa."
 (cleanTextEnglish tx1)

tx1 = "  Knots of idle-men  \
    \on the South Bridge, for 3s. 2d. .   \
    \This street named the _Via Dolorosa_."



convertTZ2nlpPrepareCall :: Bool -> Bool -> URI -> Snip -> ErrIO Doc0   -- the xml to analyzse  D -> E
-- prepare call to send text to nlp server
-- works on individual paragraphs
convertTZ2nlpPrepareCall debugNLP showXML sloc tz = do
    when debugNLP $ putIOwords ["convertTZ2nlpPrepareCall start"]
    let text = tz3text tz
    -- reduce for some special cases _italics_
    -- should be done before the split, because . of abbreviations are elliminated
    if null' text
        then return zero
        else do

            let language = tz3lang tz

            when debugNLP $ putIOwords ["convertTZ2nlpPrepareCall", "language", showT language, "\n tz", showT tz]
            case language of
                English -> englishNLP debugNLP showXML sloc text
                German -> germanNLP debugNLP showXML sloc text
                _ -> errorT ["convertTZ2nlpPrepareCall"
                    , showT language, "language has no server"]

englishNLP :: Bool -> Bool -> URI -> Text -> ErrIO Doc0
-- process an english text snip to a Doc0
englishNLP debugNLP showXML sloc text = do
    let varsEng =  [("annotators", Just "tokenize,ssplit,pos\
                                    \,lemma,ner,depparse, dcoref,coref")
        --                                    coref -coref.algorithm neural")
        -- perhaps the nerual algorithm is better, but creates problems
        -- with the xml doc received (starts with C?
        --                                        dcoref,coref")
                --                    --  coref, verlangt depparse,
                                    , ("outputFormat", Just "xml")
                                    ]
    when debugNLP $ putIOwords ["englishNLP text", showT text]

    let text2 = cleanTextEnglish  text

--            let texts = getPiece . textSplit $ text2
--            let texts = if True -- lengthChar text2 < nlpDocSizeLimit
--                            then [ text2]
--                            else getPiece nlpDocSizeLimit . textSplit2 $ text2

    docs <-  convertTZ2makeNLPCall debugNLP showXML (addPort2URI sloc 9002) varsEng  text2
--            when False $ putIOwords ["englishNLP parse"
--                    , sparse . headNote "docSents" . docSents . headNote "xx243" $ docs]
    when debugNLP $ putIOwords ["englishNLP end", showT text2]

    return   docs

cleanTextEnglish :: Text -> Text
cleanTextEnglish    = subRegex' "_([a-zA-Z ]+)_" "\\1"  -- italics even multiple words
            . subRegex' "([0-9])([ds])."  "\\1 \\2 "   -- shiling/pence
            . subRegex' "([a-zA-Z]+)-([a-zA-Z]+)" "\\1 \\2"


germanNLP :: Bool -> Bool -> URI -> Text -> ErrIO Doc0
-- process an english text snip to a Doc0
germanNLP debugNLP showXML sloc text = do
    let varsGer =  [("annotators", Just "tokenize,ssplit,pos,ner,depparse")
                                    , ("outputFormat", Just "xml")
                                    ]
    when debugNLP $ putIOwords ["germanNLP text", showT text]

    let text2 = cleanTextEnglish  text

--            let texts = getPiece . textSplit $ text2
--            let texts = if True -- lengthChar text2 < nlpDocSizeLimit
--                            then [ text2]
--                            else getPiece nlpDocSizeLimit . textSplit2 $ text2

    doc0 <-  convertTZ2makeNLPCall debugNLP showXML (addPort2URI sloc 9001 ) varsGer  text2
--            when False $ putIOwords ["germanNLP parse"
--                    , sparse . headNote "docSents" . docSents . headNote "xx243" $ docs]
--  corenlp does not lemmatize, use lemmatize service
    let sents1 = docSents doc0
    sents2 <- mapM (completeSentence False (addPort2URI sloc 17701 ) ) sents1
    let doc0' = doc0{docSents = sents2}
--    return ( doc0')
    when debugNLP $ putIOwords ["germanNLP end", showT text2]

    return   doc0'

cleanTextGerman :: Text -> Text
cleanTextGerman    = subRegex' "_([a-zA-Z ]+)_" "\\1"  -- italics even multiple words


convertTZ2makeNLPCall  :: Bool -> Bool -> URI -> [(Text,Maybe Text)] -> Text ->  ErrIO Doc0    -- the xml to analyzse  D -> E
-- call to send text to nlp server and converts xml to Doc0
-- works on individual paragraphs - but should treat bigger pieces if para is small (eg. dialog)
-- merger
convertTZ2makeNLPCall debugNLP showXML nlpServer vars text = do
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

        doc0 <- readDocString showXML xml                    -- E -> F
        when debugNLP  $
            putIOwords ["convertTZ2makeNLPCall doc0 \n", showT doc0]

        return   doc0
    `catchError` (\e -> do
         putIOwords ["convertTZ2makeNLPCall http error caught 7",  e] -- " showT msg])
         putIOwords ["convertTZ2makeNLPCall",  "text:\n",  showT text ] -- " showT msg])
--         splitAndTryAgain debugNLP showXML nlpServer vars text
         return zero
            )

