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
nlpServerEnglish  u =  addPort2URI u 9000
--            relativeTo (makeURI ":9000")     -- from Network-URI
-- not localhost!
--nlpServer = "http://nlp.gerastree.at:9000"
nlpServerGerman u     = addPort2URI u 9001
--    relativeTo (makeURI ":9001")  -- for german
nlpServerNone  = nlpServerEnglish
-- for no language which can be processed
-- should be a server just returning the input tokenized etc


test_1_C_D = testFile2File "resultBAE1" "resultD1" (map prepareTZ4nlp)
test_2_C_D = testFile2File "resultBAE2" "resultD2" (map prepareTZ4nlp)
test_3_C_D = testFile2File "resultBAE3" "resultD3" (map prepareTZ4nlp)
test_4_C_D = testFile2File "resultBAE4" "resultD4" (map prepareTZ4nlp)
test_5_C_D = testFile2File "resultBAE5" "resultD5" (map prepareTZ4nlp)
test_6_C_D = testFile2File "resultBAE6" "resultD6" (map prepareTZ4nlp)
--test_8_C_D = testFile2File "resultBAE8" "resultD8" (map prepareTZ4nlp)

-------------------------------------------------D -> E

-- only entry point !
convertTZ2nlp :: Bool -> Bool -> URI -> TZ2 -> ErrIO (Maybe (NLPtext,Doc0))   -- the xml to analyzse  D -> E
-- send a tz text to coreNLP server
-- works on individual paragraphs
convertTZ2nlp debugNLP showXML sloc tz2 = do
    when debugNLP $ putIOwords ["convertTZ2nlp start"]
    when debugNLP $ putIOwords ["convertTZ2nlp TZ2", showT tz2]
    let mtz = prepareTZ4nlp tz2
    case mtz of
        Nothing -> return Nothing
        Just tz -> do
            let language = tz3lang tz
            let text = tz3text tz
            when debugNLP $ putIOwords ["convertTZ2nlp tz", showT tz]

            let nlpServer = case language of
                            English -> nlpServerEnglish sloc
                            German -> nlpServerGerman sloc
                            NoLanguage -> nlpServerNone sloc
                            _ -> errorT ["convertTZ2nlp", showT language, "language has no server"]

            let varsEng =  [("annotators","tokenize,ssplit,pos\
                                    \,lemma,ner,depparse,dcoref,coref")
--            removed ,dcoref
--                        tokenize,ssplit,pos,lemma,ner")
        --                    -- removed ,coref, ,depparse,, coref
        -- changed to depparse, coref  instead of parse
                            , ("outputFormat","xml")
                            ]
            let varsGer =  [("annotators","tokenize,ssplit,pos,lemma,ner,parse")
--                        tokenize,ssplit,pos,lemma,ner")
        --                    -- removed ,coref, ,depparse,, coref
        -- changed to depparse, coref  instead of parse
                            , ("outputFormat","xml")
                            ]
            let vars = case language of
            -- the different parsers do not deal with all annotators well
                        German -> varsGer
                        English -> varsEng
                        _ -> varsEng

            when debugNLP $ putIOwords ["convertTZ2nlp text", showT text]
            xml ::  Text  <-   makeHttpPost7 False nlpServer vars "text/plain" text
-- german parser seems to understand utf8encoded bytestring

            when debugNLP  $ putIOwords ["convertTZ2nlp end \n", showT xml]

            doc0 <- readDocString showXML xml                    -- E -> F
            when debugNLP  $
                putIOwords ["readDocString doc0 \n", showT doc0]

            return . Just $ (tz,doc0)

testOP_C_E :: TextState2 -> [TZ2] -> ErrIO [Maybe (NLPtext,Doc0)]
testOP_C_E resultXA resultBAEfile = do
    let sloc = serverLoc  result1A

    res <- mapM (convertTZ2nlp False True sloc) resultBAEfile
    -- the secnd bool controls the rendering of the xml file
    return res

test_1_C_E = testVar3FileIO result1A "resultBAE1" "resultE1" testOP_C_E
test_2_C_E = testVar3FileIO result2A "resultBAE2" "resultE2" testOP_C_E
test_3_C_E = testVar3FileIO result3A "resultBAE3" "resultE3" testOP_C_E
test_4_C_E = testVar3FileIO result4A "resultBAE4" "resultE4" testOP_C_E
test_5_C_E = testVar3FileIO result5A "resultBAE5" "resultE5" testOP_C_E
test_6_C_E = testVar3FileIO result6A "resultBAE6" "resultE6" testOP_C_E
--test_8_C_E = testVar3FileIO result8A "resultBAE8" "resultE8" testOP_C_E

-- no test to use resultE1 and produce resultE1


-- unnecessary, inclued in C_E test
----right :: Either Text a -> a
----right (Left a) = errorT ["not a right",   a]
----right (Right a) = a
--testOP_E_F :: Bool -> [Maybe (NLPtext, Text)] -> ErrIO [Doc0]
--testOP_E_F bool result1e = do
--    let in1 :: [Text] = map (snd) . catMaybes $ result1e
----                    (result1e ::[Either Text (NLPtext, Text)])
--    mapM (readDocString False) in1
--
--test_1_E_F =
--        testVar3FileIO False "resultE1" "resultF1"  testOP_E_F
----test_1_E_F_readDocString = do   -- E -> F
----    putIOwords ["test_readDocString E -> F :  "] -- tripleResult]
----    let in1 :: [Text] = map (snd . right) (result1E ::[Either Text (NLPtext, Text)])
----    t1 <- runErr $ mapM (readDocString False) in1
----    putIOwords ["test_readDocString: result  ) ", showT  t1]
------    putIOwords ["test_parseToTZ:  result ", show' t1]
----    assertEqual resutl1F_readDocStringResult t1
--
--right :: Either Text a -> a
--right (Left a) = errorT ["not a right",   a]
--right (Right a) = a

