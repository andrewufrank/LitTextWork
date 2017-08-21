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

data NLPtext = NLPtext { tz3loc :: TextLoc
                        , tz3para :: ParaNum
                        , tz3text:: Text
                        , tz3lang :: LanguageCode }
            deriving (Read, Show, Eq )

instance Zeros NLPtext where zero = NLPtext zero zero zero zero
--instance (Zeros a) => Zeros (Maybe a) where zero = Nothing
-- todo algebra

prepareTZ4nlp :: TZ2 -> Maybe NLPtext  -- test C  -> D
-- selecte the text from TZ and convert to text
prepareTZ4nlp tz2 = if condNLPtext tz2 then Just $ formatParaText tz2
                    else Nothing

--prepareTZ4nlp = map formatParaText . filter condNLPtext
        ---------------------------------preparing for analysis

condNLPtext :: TZ2 -> Bool
-- select the paragraphs and the titles to TZtext
condNLPtext tz  = case tz of
--    TZzdahl {}  -> errorT ["condNLPtext","should not have TZzahl left", showT tz]
    TZ2markup {} ->
            case tz2tok tz of
                BuchTitel ->  True
                BuchHL1   ->  True
                BuchHL2   ->  True
                BuchHL3   ->  True
                _         ->   False
    TZ2para {} -> True

formatParaText :: TZ2 -> NLPtext
-- convert the headers to a tztext
formatParaText tz@TZ2para{} = NLPtext {
                tz3loc = tz2loc tz
                , tz3para = tz2para tz
                , tz3lang = tz2lang tz
                , tz3text = foldl1 combine2linesWithHyphenation
            . map (twm . tztext) $ (tz2tzs tz)
        }

formatParaText tz@TZ2markup {} = NLPtext {tz3loc = tz2loc tz
        , tz3lang = tz2lang tz
        , tz3para = tz2para tz
        , tz3text =  twm . tz2text $ tz}


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


--test_1_C_D = testFile2File "resultBAE1" "resultD1" (map prepareTZ4nlp)
--test_2_C_D = testFile2File "resultBAE2" "resultD2" (map prepareTZ4nlp)
--test_3_C_D = testFile2File "resultBAE3" "resultD3" (map prepareTZ4nlp)
--test_4_C_D = testFile2File "resultBAE4" "resultD4" (map prepareTZ4nlp)
--test_5_C_D = testFile2File "resultBAE5" "resultD5" (map prepareTZ4nlp)
--test_6_C_D = testFile2File "resultBAE6" "resultD6" (map prepareTZ4nlp)
--test_8_C_D = testFile2File "resultBAE8" "resultD8" (map prepareTZ4nlp)
--test_9_C_D = testFile2File "resultBAE9" "resultD9" (map prepareTZ4nlp)
--test_10_C_D = testFile2File "resultBAE10" "resultD10" (map prepareTZ4nlp)

-------------------------------------------------D -> E

-- only entry point !
convertTZ2nlp :: Bool -> Bool -> URI -> TZ2 -> ErrIO (NLPtext,[Doc0])   -- the xml to analyzse  D -> E
-- send a tz text to coreNLP server
-- works on individual paragraphs
convertTZ2nlp debugNLP showXML sloc tz2 = do
    when debugNLP $ putIOwords ["convertTZ2nlp start"]
    when False $ putIOwords ["convertTZ2nlp TZ2", showT tz2]
    let mtz = prepareTZ4nlp tz2
    case mtz of
        Nothing -> do   when debugNLP $ putIOwords ["convertTZnlp - empty text"]
                        return (zero,[])
        Just tz -> convertTZ2nlpPrepareCall debugNLP showXML sloc tz

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

convertTZ2nlpPrepareCall :: Bool -> Bool -> URI -> NLPtext -> ErrIO  (NLPtext,[Doc0])   -- the xml to analyzse  D -> E
-- prepare call to send text to nlp server
-- works on individual paragraphs
convertTZ2nlpPrepareCall debugNLP showXML sloc tz = do
    when debugNLP $ putIOwords ["convertTZ2nlpPrepareCall start"]

    let language = tz3lang tz
    let text = tz3text tz
    -- reduce for some special cases _italics_
    -- should be done before the split, because . of abbreviations are elliminated

    if null' text then return (tz,[])
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
            let texts = if lengthChar text2 < nlpDocSizeLimit
                            then [ text2]
                            else getPiece nlpDocSizeLimit . textSplit2 $ text2

            docs <- mapM (convertTZ2nlpCall debugNLP showXML nlpServer vars) texts
            when False $ putIOwords ["convertTZ2nlp parse"
                    , sparse . headNote "docSents" . docSents . headNote "xx243" $ docs]
            when debugNLP $ putIOwords ["convertTZ2nlp end", showT text2]

            return (tz, docs)

nlpDocSizeLimit = 5000  -- 18,000 gives timeout for brest

convertTZ2nlpCall  :: Bool -> Bool -> URI -> [(Text,Maybe Text)] -> Text ->  ErrIO (Doc0)    -- the xml to analyzse  D -> E
-- prepare call to send text to nlp server
-- works on individual paragraphs
convertTZ2nlpCall debugNLP showXML nlpServer vars text = do
        when True $
            putIOwords ["convertTZ2nlpCall start"
                        , showT . lengthChar $ text
                        , showT . take' 100 $ text ]
        xml ::  Text  <-   makeHttpPost7 False nlpServer "" vars
                    "multipart/form-data" text
    -- german parser seems to understand utf8encoded bytestring

        when False  $
            putIOwords ["convertTZ2nlpCall end \n", showT xml]

        doc0 <- readDocString showXML xml                    -- E -> F
        when False  $
            putIOwords ["convertTZ2nlpCall doc0 \n", showT doc0]

        return   doc0
    `catchError` (\e -> do
         putIOwords ["convertTZ2nlpCall http error caught 7",  e] -- " showT msg])
         putIOwords ["convertTZ2nlpCall",  "text:\n",  showT text ] -- " showT msg])
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

testOP_C_E :: TextDescriptor -> [TZ2] -> ErrIO   [(NLPtext,[Doc0])]
testOP_C_E resultXA resultBAEfile = do
    let sloc = nlpServer  result1A

    res <-  mapM (convertTZ2nlp False   False sloc) resultBAEfile
    -- the secnd bool controls the rendering of the xml file
    putIOwords ["testOP_C_E",  "result:\n",  showT res ] -- " showT msg])
    return res

test_1_C_E = testVar3FileIO result1A "resultBAE1" "resultE1" testOP_C_E
test_2_C_E = testVar3FileIO result2A "resultBAE2" "resultE2" testOP_C_E
test_3_C_E = testVar3FileIO result3A "resultBAE3" "resultE3" testOP_C_E
test_4_C_E = testVar3FileIO result4A "resultBAE4" "resultE4" testOP_C_E
test_5_C_E = testVar3FileIO result5A "resultBAE5" "resultE5" testOP_C_E  -- lafayette
test_6_C_E = testVar3FileIO result6A "resultBAE6" "resultE6" testOP_C_E
test_8_C_E = testVar3FileIO result8A "resultBAE8" "resultE8" testOP_C_E
test_9_C_E = testVar3FileIO result9A "resultBAE9" "resultE9" testOP_C_E
test_10_C_E = testVar3FileIO result10A "resultBAE10" "resultE10" testOP_C_E

-- no test to use resultE1 and produce resultE1

textid :: Text -> Text
textid = id


textSplit :: Text -> [Text]
--textSplit =  fromJustNote "textSplit" . splitOn' ". "
-- append . to end last piece
textSplit = fmap s2t . split (keepDelimsR $ onSublist ". "  ) . t2s
-- statt onSublis elt verwenden, so das "?." oder "!." auch brechen
-- asked on stack overflow how to combine... (coreNLP breaks on ? as well)
-- issue: splits for "costs 1s. per year" and similar
-- could be controled by merging pieces when number "s." is at end of piece
-- regular expression used in  geany could be used? is this save?

textSplit2 :: Text -> [Text]
-- split on "I" - in joyce is sort of start of a new idea..
    -- prepend I to start next piece
textSplit2 = fmap s2t . split (keepDelimsL $ onSublist " I "  ) . t2s


getPiece :: Int -> [Text] -> [Text]
-- get a piece of length < 2000
getPiece limit  = chop (takePiece limit "")

--chop :: ([a] -> (b, [a])) -> [a] -> [b]
--A useful recursion pattern for processing a list to produce a new list,
--often used for "chopping" up the input list.
--Typically chop is called with some function that will
--consume an initial prefix of the list and produce a value and the rest of the list.

takePiece :: Int -> Text  -> [Text] ->   (Text, [Text])
takePiece _ b [] = (b,[])
takePiece limit b (a:as)
    | null' b && lengthChar a > limit = (a, as)
    | lengthChar ab > limit = (b, a:as)
    | otherwise = takePiece limit ab as  -- accumulate a larger piece
                    where
                        ab = concat' [b, " ", a]
--                        limit = 12000

--test_longText1 ::  IO ()
--test_longText1 = testFile2File "longtext.txt" "lt1" textid
--test_split = testFile2File "lt1" "lt2" textSplit
--test_chop = testFile2File "lt2" "lt3" getPiece

-- test with limit 5
--test_getPiece1 = assertEqual  ["abcdefg", " hik"] (getPiece ["abcdefg", "hik"])
--test_getPiece2 = assertEqual [" abc", " defg", " hik"] (getPiece ["abc","defg", "hik"])
--test_getPiece3 = assertEqual  [" AB", "abcdefg", " hik"] (getPiece ["AB","abcdefg", "hik"])
--test_getPiece4 = assertEqual  [" AB", " abc", " d ef", " g hi", " k"]
--            (getPiece ["AB","abc", "d", "ef","g", "hi","k"])

