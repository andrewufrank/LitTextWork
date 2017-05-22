-----------------------------------------------------------------------------
--
-- Module      :  Parser . Produce NLP  - betteer useNLPprocessors
-- Copyright   :  andrew u frank -
--
-- | analyzes one paragraph

-- version 2 assumes that each paragraph is individually analyzed -
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}

module Parser.ProduceNLP
    (module Parser.ProduceNLP
    , module CoreNLP.Defs0
--    (convertTZ2nlp
--    , prepareTZ4nlp
--    , ErrIO (..)
--    , htf_thisModulesTests
--    , result1E_nlpResult
    ) where

import           Test.Framework

--import           CoreNLP.Snippets2nt          (makeNLPrequest5) -- , readDocString)
import           Data.RDF
import Data.Maybe -- todo
import           Parser.Foundation  -- todo should be comming up
import Lines2para.Lines2para
import Lines2para.HandleLayout
import Parser.ReadMarkupAB  -- todo
import Uniform.Error  -- todo should be comming up
import Uniform.HttpGet
import           CoreNLP.Snippets2nt    --      (readDocString)
import           CoreNLP.Defs0

debugNLP = False

data NLPtext = NLPtext { tz3loc :: TextLoc
                    , tz3text:: Text
                    , tz3lang :: LanguageCode }
            deriving (Show, Eq )

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
--                    buchEnde ... errorT ["ProduceLit.hs conv2", "missing Markup case", showT . tztok $ tz]
--    TZleer {} -> False  -- errorT ["condNLPtext","should not haveleer", showT tz]
--    TZtext {} -> errorT ["condNLPtext","should not have single text", showT tz]
--    TZkurz {} -> errorT ["condNLPtext","should not have single kurz", showT tz]
    TZ2para {} -> True

--    TZkurz {} -> p : condNLPtext rest
--                        where (p,rest) = collectKurz (t:ts)
--    _ -> errorT ["ProduceLit.hs conv2", "missing TZ case", showT tz]

formatParaText :: TZ2 -> NLPtext
-- convert the headers to a tztext
formatParaText tz@TZ2para{} = NLPtext {tz3loc = tz2loc tz, tz3lang = tz2lang tz
        , tz3text = foldl1 combine2linesWithHyphenation
            . map (twm . tztext) $ (tz2tzs tz)
        }

formatParaText tz@TZ2markup {} = NLPtext {tz3loc = tz2loc tz
        , tz3lang = tz2lang tz
        , tz3text =  twm . tz2text $ tz}

--serverLoc = localhost
--serverbrest = "nlp.gerastree.at"
--localhost = "127.0.0.1"

nlpServerEnglish, nlpServerGerman, nlpServerNone ::PartURI -> PartURI
nlpServerEnglish loc =   loc <> ":9000"  -- not localhost!
--nlpServer = "http://nlp.gerastree.at:9000"
nlpServerGerman loc =   loc <> ":9001"  -- for german
nlpServerNone loc = nlpServerEnglish loc
-- for no language which can be processed
-- should be a server just returning the input tokenized etc

----------------
test_C_D :: IO ()  -- C -> D
test_C_D =  do
    putIOwords ["prepareTZ4nlp:   "] -- tzResult]
    let t1 = map prepareTZ4nlp result1BAE
    assertEqual result1D (catMaybes t1)

-------------------------------------------------D -> E

-- only entry point !
convertTZ2nlp :: Bool -> PartURI -> TZ2 -> ErrIO (Maybe (NLPtext,Doc0))   -- the xml to analyzse  D -> E
-- send a tz text to coreNLP server
-- works on individual paragraphs
convertTZ2nlp showXML sloc tz2 = do
    when debugNLP $ putIOwords ["convertTZ2nlp"]
    let mtz = prepareTZ4nlp tz2
    case mtz of
        Nothing -> return Nothing
        Just tz -> do
            let language = tz3lang tz
            let text = tz3text tz

            let nlpServer = case language of
                            English -> nlpServerEnglish sloc
                            German -> nlpServerGerman sloc
                            NoLanguage -> nlpServerNone sloc
                            _ -> errorT ["convertTZ2nlp", showT language, "language has no server"]

            let vars =  [("annotators","tokenize,ssplit,pos,lemma,ner,parse")
        --                    -- removed ,coref
                            , ("outputFormat","xml")
                            ]
            xml ::  Text  <-   makeHttpPost7 False nlpServer vars "text/plain" text
-- german parser seems to understand utf8encoded bytestring

            when debugNLP  $ putIOwords ["convertTZ2nlp end \n", showT xml]

            doc0 <- readDocString showXML xml                    -- E -> F

            return . Just $ (tz,doc0)


test_1_C_E  ::   IO ()  -- D -> E
test_1_C_E  =  do
    putIOwords ["convertTZ2nlp: result1D to result1E  "] -- tzResult]
    let sloc = serverLoc result1A
    res <- mapM (runErr . convertTZ2nlp False sloc) result1BAE
    assertEqual result1E res

result1E =
    [Right Nothing,
     Right
       (Just
          (NLPtext{tz3loc = TextLoc{tlpage = "11", tlline = 6},
                   tz3text = "(Krieg f\252r Welt)", tz3lang = German},
           Doc0{docSents =
                  [Sentence0{sid = SentID0{unSentID0 = 1},
                             sparse =
                               "(ROOT\n  (NUR\n    (S\n      (NP\n        (CNP (TRUNC -LRB-) (NN Krieg))\n        (PP (APPR f\252r) (NN Welt)))\n      (VP\n        (CVP\n          (VP (TRUNC -RRB-)))))))\n\n",
                             stoks =
                               [Token0{tid = TokenID0{untid0 = 1},
                                       tword = Wordform0{word0 = "-LRB-"},
                                       tlemma = Lemma0{lemma0 = "-lrb-"}, tbegin = 0, tend = 1,
                                       tpos = Unk, tpostt = "", tner = ["O"], tspeaker = []},
                                Token0{tid = TokenID0{untid0 = 2},
                                       tword = Wordform0{word0 = "Krieg"},
                                       tlemma = Lemma0{lemma0 = "krieg"}, tbegin = 1, tend = 6,
                                       tpos = NN, tpostt = "", tner = ["O"], tspeaker = []},
                                Token0{tid = TokenID0{untid0 = 3},
                                       tword = Wordform0{word0 = "f\252r"},
                                       tlemma = Lemma0{lemma0 = "f\252r"}, tbegin = 7, tend = 10,
                                       tpos = Unk, tpostt = "", tner = ["O"], tspeaker = []},
                                Token0{tid = TokenID0{untid0 = 4},
                                       tword = Wordform0{word0 = "Welt"},
                                       tlemma = Lemma0{lemma0 = "welt"}, tbegin = 11, tend = 15,
                                       tpos = NN, tpostt = "", tner = ["O"], tspeaker = []},
                                Token0{tid = TokenID0{untid0 = 5},
                                       tword = Wordform0{word0 = "-RRB-"},
                                       tlemma = Lemma0{lemma0 = "-rrb-"}, tbegin = 15, tend = 16,
                                       tpos = Unk, tpostt = "", tner = ["O"], tspeaker = []}],
                             sdeps = Nothing}],
                docCorefs = []})),
     Right
       (Just
          (NLPtext{tz3loc = TextLoc{tlpage = "12", tlline = 8},
                   tz3text = "Unsere Br\228uche werden lebendig", tz3lang = German},
           Doc0{docSents =
                  [Sentence0{sid = SentID0{unSentID0 = 1},
                             sparse =
                               "(ROOT\n  (NUR\n    (S\n      (NP (PPOSAT Unsere) (NN Br\228uche))\n      (VAFIN werden) (ADJD lebendig))))\n\n",
                             stoks =
                               [Token0{tid = TokenID0{untid0 = 1},
                                       tword = Wordform0{word0 = "Unsere"},
                                       tlemma = Lemma0{lemma0 = "unsere"}, tbegin = 0, tend = 6,
                                       tpos = Unk, tpostt = "", tner = ["O"], tspeaker = []},
                                Token0{tid = TokenID0{untid0 = 2},
                                       tword = Wordform0{word0 = "Br\228uche"},
                                       tlemma = Lemma0{lemma0 = "br\228uche"}, tbegin = 7,
                                       tend = 14, tpos = NN, tpostt = "", tner = ["O"],
                                       tspeaker = []},
                                Token0{tid = TokenID0{untid0 = 3},
                                       tword = Wordform0{word0 = "werden"},
                                       tlemma = Lemma0{lemma0 = "werden"}, tbegin = 15, tend = 21,
                                       tpos = Unk, tpostt = "", tner = ["O"], tspeaker = []},
                                Token0{tid = TokenID0{untid0 = 4},
                                       tword = Wordform0{word0 = "lebendig"},
                                       tlemma = Lemma0{lemma0 = "lebendig"}, tbegin = 22, tend = 30,
                                       tpos = Unk, tpostt = "", tner = ["O"], tspeaker = []}],
                             sdeps = Nothing}],
                docCorefs = []})),
     Right
       (Just
          (NLPtext{tz3loc = TextLoc{tlpage = "13", tlline = 10},
                   tz3text =
                     "Was w\252rde ihm fremd und was m\246chte sein eigen sein in C\233rb\232re?",
                   tz3lang = German},
           Doc0{docSents =
                  [Sentence0{sid = SentID0{unSentID0 = 1},
                             sparse =
                               "(ROOT\n  (CS\n    (S\n      (NP (PWS Was))\n      (VAFIN w\252rde) (PPER ihm) (ADJD fremd))\n    (KON und)\n    (S (PWS was) (VMFIN m\246chte)\n      (VP\n        (NP\n          (CNP\n            (NP (PPOSAT sein)\n              (CNP\n                (NP\n                  (AP (ADJD eigen)))))))\n        (VAINF sein)\n        (PP (APPR in) (NN C\233rb\232re))))\n    ($. ?)))\n\n",
                             stoks =
                               [Token0{tid = TokenID0{untid0 = 1},
                                       tword = Wordform0{word0 = "Was"},
                                       tlemma = Lemma0{lemma0 = "was"}, tbegin = 0, tend = 3,
                                       tpos = Unk, tpostt = "", tner = ["O"], tspeaker = []},
                                Token0{tid = TokenID0{untid0 = 2},
                                       tword = Wordform0{word0 = "w\252rde"},
                                       tlemma = Lemma0{lemma0 = "w\252rde"}, tbegin = 4, tend = 9,
                                       tpos = Unk, tpostt = "", tner = ["O"], tspeaker = []},
                                Token0{tid = TokenID0{untid0 = 3},
                                       tword = Wordform0{word0 = "ihm"},
                                       tlemma = Lemma0{lemma0 = "ihm"}, tbegin = 10, tend = 13,
                                       tpos = Unk, tpostt = "", tner = ["O"], tspeaker = []},
                                Token0{tid = TokenID0{untid0 = 4},
                                       tword = Wordform0{word0 = "fremd"},
                                       tlemma = Lemma0{lemma0 = "fremd"}, tbegin = 14, tend = 19,
                                       tpos = Unk, tpostt = "", tner = ["O"], tspeaker = []},
                                Token0{tid = TokenID0{untid0 = 5},
                                       tword = Wordform0{word0 = "und"},
                                       tlemma = Lemma0{lemma0 = "und"}, tbegin = 20, tend = 23,
                                       tpos = Unk, tpostt = "", tner = ["O"], tspeaker = []},
                                Token0{tid = TokenID0{untid0 = 6},
                                       tword = Wordform0{word0 = "was"},
                                       tlemma = Lemma0{lemma0 = "was"}, tbegin = 24, tend = 27,
                                       tpos = Unk, tpostt = "", tner = ["O"], tspeaker = []},
                                Token0{tid = TokenID0{untid0 = 7},
                                       tword = Wordform0{word0 = "m\246chte"},
                                       tlemma = Lemma0{lemma0 = "m\246chte"}, tbegin = 28,
                                       tend = 34, tpos = Unk, tpostt = "", tner = ["O"],
                                       tspeaker = []},
                                Token0{tid = TokenID0{untid0 = 8},
                                       tword = Wordform0{word0 = "sein"},
                                       tlemma = Lemma0{lemma0 = "sein"}, tbegin = 35, tend = 39,
                                       tpos = Unk, tpostt = "", tner = ["O"], tspeaker = []},
                                Token0{tid = TokenID0{untid0 = 9},
                                       tword = Wordform0{word0 = "eigen"},
                                       tlemma = Lemma0{lemma0 = "eigen"}, tbegin = 40, tend = 45,
                                       tpos = Unk, tpostt = "", tner = ["O"], tspeaker = []},
                                Token0{tid = TokenID0{untid0 = 10},
                                       tword = Wordform0{word0 = "sein"},
                                       tlemma = Lemma0{lemma0 = "sein"}, tbegin = 46, tend = 50,
                                       tpos = Unk, tpostt = "", tner = ["O"], tspeaker = []},
                                Token0{tid = TokenID0{untid0 = 11},
                                       tword = Wordform0{word0 = "in"},
                                       tlemma = Lemma0{lemma0 = "in"}, tbegin = 51, tend = 53,
                                       tpos = Unk, tpostt = "", tner = ["O"], tspeaker = []},
                                Token0{tid = TokenID0{untid0 = 12},
                                       tword = Wordform0{word0 = "C\233rb\232re"},
                                       tlemma = Lemma0{lemma0 = "c\233rb\232re"}, tbegin = 54,
                                       tend = 61, tpos = NN, tpostt = "", tner = ["I-LOC"],
                                       tspeaker = []},
                                Token0{tid = TokenID0{untid0 = 13}, tword = Wordform0{word0 = "?"},
                                       tlemma = Lemma0{lemma0 = "?"}, tbegin = 61, tend = 62,
                                       tpos = Unk, tpostt = "", tner = ["O"], tspeaker = []}],
                             sdeps = Nothing}],
                docCorefs = []})),
     Right
       (Just
          (NLPtext{tz3loc = TextLoc{tlpage = "13", tlline = 12},
                   tz3text = "Er fragte sich als zweiter Paragraph.",
                   tz3lang = German},
           Doc0{docSents =
                  [Sentence0{sid = SentID0{unSentID0 = 1},
                             sparse =
                               "(ROOT\n  (S (PPER Er) (VVFIN fragte) (PRF sich)\n    (PP (APPR als) (ADJA zweiter) (NN Paragraph))\n    ($. .)))\n\n",
                             stoks =
                               [Token0{tid = TokenID0{untid0 = 1},
                                       tword = Wordform0{word0 = "Er"},
                                       tlemma = Lemma0{lemma0 = "er"}, tbegin = 0, tend = 2,
                                       tpos = Unk, tpostt = "", tner = ["O"], tspeaker = []},
                                Token0{tid = TokenID0{untid0 = 2},
                                       tword = Wordform0{word0 = "fragte"},
                                       tlemma = Lemma0{lemma0 = "fragte"}, tbegin = 3, tend = 9,
                                       tpos = Unk, tpostt = "", tner = ["O"], tspeaker = []},
                                Token0{tid = TokenID0{untid0 = 3},
                                       tword = Wordform0{word0 = "sich"},
                                       tlemma = Lemma0{lemma0 = "sich"}, tbegin = 10, tend = 14,
                                       tpos = Unk, tpostt = "", tner = ["O"], tspeaker = []},
                                Token0{tid = TokenID0{untid0 = 4},
                                       tword = Wordform0{word0 = "als"},
                                       tlemma = Lemma0{lemma0 = "als"}, tbegin = 15, tend = 18,
                                       tpos = Unk, tpostt = "", tner = ["O"], tspeaker = []},
                                Token0{tid = TokenID0{untid0 = 5},
                                       tword = Wordform0{word0 = "zweiter"},
                                       tlemma = Lemma0{lemma0 = "zweiter"}, tbegin = 19, tend = 26,
                                       tpos = Unk, tpostt = "", tner = ["O"], tspeaker = []},
                                Token0{tid = TokenID0{untid0 = 6},
                                       tword = Wordform0{word0 = "Paragraph"},
                                       tlemma = Lemma0{lemma0 = "paragraph"}, tbegin = 27,
                                       tend = 36, tpos = NN, tpostt = "", tner = ["O"],
                                       tspeaker = []},
                                Token0{tid = TokenID0{untid0 = 7}, tword = Wordform0{word0 = "."},
                                       tlemma = Lemma0{lemma0 = "."}, tbegin = 36, tend = 37,
                                       tpos = Unk, tpostt = "", tner = ["O"], tspeaker = []}],
                             sdeps = Nothing}],
                docCorefs = []}))]
result1D =
    [NLPtext{tz3loc = TextLoc{tlpage = "11", tlline = 6},
             tz3text = "(Krieg f\252r Welt)", tz3lang = German},
     NLPtext{tz3loc = TextLoc{tlpage = "12", tlline = 8},
             tz3text = "Unsere Br\228uche werden lebendig", tz3lang = German},
     NLPtext{tz3loc = TextLoc{tlpage = "13", tlline = 10},
             tz3text =
               "Was w\252rde ihm fremd und was m\246chte sein eigen sein in C\233rb\232re?",
             tz3lang = German},
     NLPtext{tz3loc = TextLoc{tlpage = "13", tlline = 12},
             tz3text = "Er fragte sich als zweiter Paragraph.",
             tz3lang = German}]

