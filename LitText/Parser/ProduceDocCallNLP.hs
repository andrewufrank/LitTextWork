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
import Uniform.HttpCallWithConduit (makeHttpPost7, addPort2URI)
import Text.Regex (mkRegex, subRegex)
import Parser.FilterTextForNLP

snip2doc :: Bool -> Bool -> URI -> Snip -> ErrIO   Doc0     -- the xml to analyzse  D -> E
---- ^ the entry point for conversionm of tz to Doc0
snip2doc debugNLP showXML sloc snip = do
    when debugNLP $ putIOwords ["snip2doc start"]
    when False $ putIOwords ["snip2doc snip", showT snip]
    convertTZ2nlpPrepareCall debugNLP showXML sloc snip
--

--convertTZ2nlp :: Bool -> Bool -> URI -> TZ2 -> ErrIO (NLPtext,[Doc0])   -- the xml to analyzse  D -> E
---- ^ the entry point for conversionm of tz to Doc0
--convertTZ2nlp debugNLP showXML sloc tz2 = do
--    when debugNLP $ putIOwords ["convertTZ2nlp start"]
--    when False $ putIOwords ["convertTZ2nlp TZ2", showT tz2]
--    let nlptext = prepareTZ4nlp tz2
--    case nlptext of
--        Nothing -> do   when debugNLP $ putIOwords ["convertTZnlp - empty text"]
--                        return (zero,[])
--        Just tz -> convertTZ2nlpPrepareCall debugNLP showXML sloc tz
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

nlpServerEnglish, nlpServerGerman, nlpServerNone :: URI ->  URI
nlpServerEnglish  u =  addPort2URI u 9002
-- changed to 9002 to avoid clash with live 9000 which is the same for all
--            relativeTo (makeURI ":9000")     -- from Network-URI
-- not localhost!
--nlpServer = "http://nlp.gerastree.at:9000"
nlpServerGerman u     = addPort2URI u 9001
--    relativeTo (makeURI ":9001")  -- for german
nlpServerNone  = nlpServerEnglish
-- for no language which can be processed
-- should be a server just returning the input tokenized etc



------------------------------------------------ regroup paragraphs
-- goal - avoid small paragraphs in dialog to have coref work
--      avoid very long paragraphs (problematic - how to split?)
-- splitting is curently switched off


-- not possible, because only one para available here


cleanText  :: LanguageCode -> Text -> Text
-- ^ replace some special stuff which causes troubles
-- language specific
-- eg italics marks, 9s. or 4d. or row-house
cleanText language text = case language of
                            English -> cleanTextEnglish text
                            German -> cleanTextGerman text
                            _ -> text
    where
        cleanTextEnglish :: Text -> Text
        cleanTextEnglish    = subRegex' "_([a-zA-Z ]+)_" "\\1"  -- italics even multiple words
                    . subRegex' "([0-9])([ds])."  "\\1 \\2 "   -- shiling/pence
                    . subRegex' "([a-zA-Z]+)-([a-zA-Z]+)" "\\1 \\2"
                                    -- two-words,split with blank

        cleanTextGerman :: Text -> Text
        cleanTextGerman    = subRegex' "_([a-zA-Z ]+)_" "\\1"  -- italics even multiple words


subRegex' :: Text -> Text -> Text -> Text
-- replace the in the t the regex with the replacement
subRegex' reg rep t = s2t $ subRegex (mkRegex . t2s $ reg) (t2s t) (t2s rep)

test_clean1 = assertEqual "The father went to the row house for 2 d  and got it."
        (cleanText English "The _father_ went to the row-house for 2d. and got it.")
test_clean2 = assertEqual "  Knots of idle men  on the South Bridge, for 3 s  2 d  .\
        \   This street named the Via Dolorosa."
 (cleanText English tx1)

tx1 = "  Knots of idle-men  \
    \on the South Bridge, for 3s. 2d. .   \
    \This street named the _Via Dolorosa_."



convertTZ2nlpPrepareCall :: Bool -> Bool -> URI -> Snip -> ErrIO Doc0   -- the xml to analyzse  D -> E
-- prepare call to send text to nlp server
-- works on individual paragraphs
convertTZ2nlpPrepareCall debugNLP showXML sloc tz = do
    when debugNLP $ putIOwords ["convertTZ2nlpPrepareCall start"]

    let language = tz3lang tz
    let text = tz3text tz
    -- reduce for some special cases _italics_
    -- should be done before the split, because . of abbreviations are elliminated

    if null' text then return zero
        else do
            when debugNLP $ putIOwords ["convertTZ2nlpPrepareCall tz", showT tz]

            let nlpServer = case language of
                            English -> nlpServerEnglish sloc
                            German -> nlpServerGerman sloc
                            NoLanguage -> nlpServerNone sloc
                            _ -> errorT ["convertTZ2nlpPrepareCall"
                                , showT language, "language has no server"]

            let varsEng =  [("annotators", Just "tokenize,ssplit,pos\
                            \,lemma,ner,depparse, dcoref,coref")
--                                    coref -coref.algorithm neural")
-- perhaps the nerual algorithm is better, but creates problems
-- with the xml doc received (starts with C?
--                                        dcoref,coref")
        --                    --  coref, verlangt depparse,
                            , ("outputFormat", Just "xml")
                            ]
            let varsGer =  [("annotators", Just "tokenize,ssplit,pos,ner,depparse")
        --
        -- german only ssplit, pos, ner, depparse
        -- coref and dcoref crash for corenlp
                            , ("outputFormat",Just "xml")
                            ]
    --      see https://stanfordnlp.github.io/CoreNLP/human-languages.html
            let vars = case language of
            -- the different parsers do not deal with all annotators well
                        German -> varsGer
                        English -> varsEng
                        _ -> varsEng

            when debugNLP $ putIOwords ["convertTZ2nlp text", showT text]

            let text2 = cleanText language text

--            let texts = getPiece . textSplit $ text2
--            let texts = if True -- lengthChar text2 < nlpDocSizeLimit
--                            then [ text2]
--                            else getPiece nlpDocSizeLimit . textSplit2 $ text2

            docs <-  convertTZ2makeNLPCall debugNLP showXML nlpServer vars  text2
--            when False $ putIOwords ["convertTZ2nlp parse"
--                    , sparse . headNote "docSents" . docSents . headNote "xx243" $ docs]
            when debugNLP $ putIOwords ["convertTZ2nlp end", showT text2]

            return   docs

--nlpDocSizeLimit = 5000  -- 18,000 gives timeout for brest
-- there seems to be an issue with texts which are exactly 5000 and the next piece is
-- then empty, which then loops infinitely calling nlp with input ""

convertTZ2makeNLPCall  :: Bool -> Bool -> URI -> [(Text,Maybe Text)] -> Text ->  ErrIO (Doc0)    -- the xml to analyzse  D -> E
-- call to send text to nlp server and converts xml to Doc0
-- works on individual paragraphs - but should treat bigger pieces if para is small (eg. dialog)
-- merger
convertTZ2makeNLPCall debugNLP showXML nlpServer vars text = do
        when debugNLP $
            putIOwords ["convertTZ2makeNLPCall start"
                        , showT . lengthChar $ text
                        , showT . take' 100 $ text ]
        xml ::  Text  <-   makeHttpPost7 debugNLP nlpServer "" vars
                    "multipart/form-data" text
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
            -- hier die aufteilung des texts mit breakOnAll ". " und
            -- dann drop wihle (\x -> length' . fst $ x <- lenght' . snd $ x)
            -- wenn nichts uebrig, dann take last. wenn gleichgross give up
            -- dann die resultate einzeln bearbeiten!

--splitAndTryAgain :: Bool -> Bool -> URI -> [(Text,Text)] -> Text -> ErrIO [(Doc0)]
---- split the text in two and try each
--splitAndTryAgain debugNLP showXML nlpServer vars text = do
--    when debugNLP $ putIOwords ["splitAndTryAgain start"]
--    -- this will not be used
--    return []

--textid :: Text -> Text
--textid = id
--
--
--textSplit :: Text -> [Text]
----textSplit =  fromJustNote "textSplit" . splitOn' ". "
---- append . to end last piece
--textSplit = fmap s2t . split (keepDelimsR $ onSublist ". "  ) . t2s
---- statt onSublis elt verwenden, so das "?." oder "!." auch brechen
---- asked on stack overflow how to combine... (coreNLP breaks on ? as well)
---- issue: splits for "costs 1s. per year" and similar
---- could be controled by merging pieces when number "s." is at end of piece
---- regular expression used in  geany could be used? is this save?
--
--textSplit2 :: Text -> [Text]
---- split on "I" - in joyce is sort of start of a new idea..
--    -- prepend I to start next piece
--textSplit2 = fmap s2t . split (keepDelimsL $ onSublist " I "  ) . t2s
--
--
--getPiece :: Int -> [Text] -> [Text]
---- get a piece of length < 2000
--getPiece limit  = chop (takePiece limit "")
--
----chop :: ([a] -> (b, [a])) -> [a] -> [b]
----A useful recursion pattern for processing a list to produce a new list,
----often used for "chopping" up the input list.
----Typically chop is called with some function that will
----consume an initial prefix of the list and produce a value and the rest of the list.
--
--takePiece :: Int -> Text  -> [Text] ->   (Text, [Text])
--takePiece _ b [] = (b,[])
--takePiece limit b (a:as)
--    | null' b && lengthChar a > limit = (a, as)
--    | lengthChar ab > limit = (b, a:as)
--    | otherwise = takePiece limit ab as  -- accumulate a larger piece
--                    where
--                        ab = concat' [b, " ", a]
----                        limit = 12000
--
----test_longText1 ::  IO ()
----test_longText1 = testFile2File "longtext.txt" "lt1" textid
----test_split = testFile2File "lt1" "lt2" textSplit
----test_chop = testFile2File "lt2" "lt3" getPiece
--
---- test with limit 5
----test_getPiece1 = assertEqual  ["abcdefg", " hik"] (getPiece ["abcdefg", "hik"])
----test_getPiece2 = assertEqual [" abc", " defg", " hik"] (getPiece ["abc","defg", "hik"])
----test_getPiece3 = assertEqual  [" AB", "abcdefg", " hik"] (getPiece ["AB","abcdefg", "hik"])
----test_getPiece4 = assertEqual  [" AB", " abc", " d ef", " g hi", " k"]
----            (getPiece ["AB","abc", "d", "ef","g", "hi","k"])

