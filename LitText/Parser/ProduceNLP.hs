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
    -- , module CoreNLP.Snippets2nt
--    (convertTZ2nlp
--    , prepareTZ4nlp
--    , ErrIO (..)
--    , htf_thisModulesTests
--    , result1E_nlpResult
    ) where

import           Test.Framework

--import           CoreNLP.Snippets2nt          (makeNLPrequest5) -- , readDocString)
-- import           Data.RDF
import Data.Maybe -- todo
-- import           Parser.Foundation  -- todo should be comming up
import Lines2para.Lines2para
import Lines2para.HandleLayout
import Parser.ReadMarkupAB  -- todo
-- import Uniform.Error  -- todo should be comming up
-- import Uniform.HttpGet
import Producer.Servers
-- import           Text.XML.HXT.Core       hiding (when)

-- import           CoreNLP.Snippets2nt    --      (readDocString)
import           CoreNLP.Defs0
import CoreNLP.CoreNLPxml


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
    TZ2para {} -> True

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

----------------
test_C_D :: IO ()  -- C -> D
test_C_D =  do
    putIOwords ["prepareTZ4nlp:   "] -- tzResult]
    let t1 = map prepareTZ4nlp result1BAE
    assertEqual result1D (catMaybes t1)

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

            let varsEng =  [("annotators","tokenize,ssplit,pos,lemma,ner,depparse,dcoref")
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

            when debugNLP $ putIOwords ["convertTZ2nlp text", showT text]
            xml ::  Text  <-   makeHttpPost7 False nlpServer vars "text/plain" text
-- german parser seems to understand utf8encoded bytestring

            when debugNLP  $ putIOwords ["convertTZ2nlp end \n", showT xml]

            doc0 <- readDocString showXML xml                    -- E -> F

            return . Just $ (tz,doc0)


test_1_C_E  ::   IO ()  -- D -> E
test_1_C_E  =  do
    putIOwords ["convertTZ2nlp: result1D to result1E  "] -- tzResult]
    let sloc = serverLoc  result1A
    putIOwords ["test_1_C_E server location is ", showT sloc]
    putIOwords ["test_1_C_E server input is ", showT result1BAE]

    res <- runErr $ mapM (convertTZ2nlp False False sloc) result1BAE
    assertEqual result1E res

--test_6_C_E  ::   IO ()  -- D -> E
--test_6_C_E  =  do
--    putIOwords ["convertTZ2nlp: result6D to result6E  "] -- tzResult]
--    let sloc = serverLoc  result6A
--    putIOwords ["test_6_C_E server location is ", showT sloc]
--    putIOwords ["test_6_C_E server input is ", showT result6BAE]
--
--    res <- runErr $ mapM (convertTZ2nlp False False sloc) result6BAE
--    assertEqual result6E res
--
--result6E =
--    Right
--      [Just
--         (NLPtext{tz3loc = TextLoc{tlpage = "", tlline = 25},
--                  tz3text = "ALICE'S ADVENTURES IN WONDERLAND", tz3lang = English},
--          Doc0{docSents =
--                 [Sentence0{sid = SentID0{unSentID0 = 1},
--                            sparse =
--                              "(ROOT\n  (X\n    (NP\n      (NP\n        (NP (NNP ALICE) (POS 'S))\n        (NNP ADVENTURES))\n      (PP (IN IN)\n        (NP (NNP WONDERLAND))))))\n\n",
--                            stoks =
--                              [Token0{tid = TokenID0{untid0 = 1},
--                                      tword = Wordform0{word0 = "ALICE"},
--                                      tlemma = Lemma0{lemma0 = "ALICE"}, tbegin = 0, tend = 5,
--                                      tpos = NNP, tpostt = "", tner = ["PERSON"],
--                                      tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 2}, tword = Wordform0{word0 = "'S"},
--                                      tlemma = Lemma0{lemma0 = "'s"}, tbegin = 5, tend = 7,
--                                      tpos = POS, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 3},
--                                      tword = Wordform0{word0 = "ADVENTURES"},
--                                      tlemma = Lemma0{lemma0 = "ADVENTURES"}, tbegin = 8, tend = 18,
--                                      tpos = NNP, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 4}, tword = Wordform0{word0 = "IN"},
--                                      tlemma = Lemma0{lemma0 = "in"}, tbegin = 19, tend = 21,
--                                      tpos = IN, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 5},
--                                      tword = Wordform0{word0 = "WONDERLAND"},
--                                      tlemma = Lemma0{lemma0 = "WONDERLAND"}, tbegin = 22,
--                                      tend = 32, tpos = NNP, tpostt = "", tner = ["O"],
--                                      tspeaker = [PER0]}],
--                            sdeps =
--                              Just
--                                (DependenceType0{dtt = "enhanced-plus-plus-dependencies",
--                                                 dtd =
--                                                   [Dependence0{dtype =
--                                                                  DepCode{d1 = ROOT, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 0},
--                                                                      dword =
--                                                                        Wordform0{word0 = "ROOT"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 3},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "ADVENTURES"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = NMOD, d2 = POSS},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 3},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "ADVENTURES"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 1},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "ALICE"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = CASE, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 1},
--                                                                      dword =
--                                                                        Wordform0{word0 = "ALICE"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 2},
--                                                                      dword =
--                                                                        Wordform0{word0 = "'S"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = CASE, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 5},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "WONDERLAND"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 4},
--                                                                      dword =
--                                                                        Wordform0{word0 = "IN"}}},
--                                                    Dependence0{dtype = DepCode{d1 = NMOD, d2 = IN},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 3},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "ADVENTURES"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 5},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "WONDERLAND"}}}]})}],
--               docCorefs = []}),
--       Nothing, Nothing,
--       Just
--         (NLPtext{tz3loc = TextLoc{tlpage = "", tlline = 32},
--                  tz3text = "CHAPTER I. Down the Rabbit-Hole", tz3lang = English},
--          Doc0{docSents =
--                 [Sentence0{sid = SentID0{unSentID0 = 1},
--                            sparse =
--                              "(ROOT\n  (FRAG\n    (NP (NN CHAPTER) (NN I.))\n    (PP (IN Down)\n      (NP (DT the) (NN Rabbit-Hole)))))\n\n",
--                            stoks =
--                              [Token0{tid = TokenID0{untid0 = 1},
--                                      tword = Wordform0{word0 = "CHAPTER"},
--                                      tlemma = Lemma0{lemma0 = "chapter"}, tbegin = 0, tend = 7,
--                                      tpos = NN, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 2}, tword = Wordform0{word0 = "I."},
--                                      tlemma = Lemma0{lemma0 = "i."}, tbegin = 8, tend = 10,
--                                      tpos = NN, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 3},
--                                      tword = Wordform0{word0 = "Down"},
--                                      tlemma = Lemma0{lemma0 = "down"}, tbegin = 11, tend = 15,
--                                      tpos = IN, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 4},
--                                      tword = Wordform0{word0 = "the"},
--                                      tlemma = Lemma0{lemma0 = "the"}, tbegin = 16, tend = 19,
--                                      tpos = DT, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 5},
--                                      tword = Wordform0{word0 = "Rabbit-Hole"},
--                                      tlemma = Lemma0{lemma0 = "rabbit-hole"}, tbegin = 20,
--                                      tend = 31, tpos = NN, tpostt = "", tner = ["MISC"],
--                                      tspeaker = [PER0]}],
--                            sdeps =
--                              Just
--                                (DependenceType0{dtt = "enhanced-plus-plus-dependencies",
--                                                 dtd =
--                                                   [Dependence0{dtype =
--                                                                  DepCode{d1 = ROOT, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 0},
--                                                                      dword =
--                                                                        Wordform0{word0 = "ROOT"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 2},
--                                                                      dword =
--                                                                        Wordform0{word0 = "I."}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = COMPOUND,
--                                                                          d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 2},
--                                                                      dword =
--                                                                        Wordform0{word0 = "I."}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 1},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "CHAPTER"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = CASE, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 5},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "Rabbit-Hole"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 3},
--                                                                      dword =
--                                                                        Wordform0{word0 = "Down"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = DET, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 5},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "Rabbit-Hole"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 4},
--                                                                      dword =
--                                                                        Wordform0{word0 = "the"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = NMOD, d2 = DOWN},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 2},
--                                                                      dword =
--                                                                        Wordform0{word0 = "I."}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 5},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "Rabbit-Hole"}}}]})}],
--               docCorefs = []}),
--       Just
--         (NLPtext{tz3loc = TextLoc{tlpage = "", tlline = 34},
--                  tz3text =
--                    "Alice was beginning to get very tired of sitting by her sister on the bank.",
--                  tz3lang = English},
--          Doc0{docSents =
--                 [Sentence0{sid = SentID0{unSentID0 = 1},
--                            sparse =
--                              "(ROOT\n  (S\n    (NP (NNP Alice))\n    (VP (VBD was)\n      (VP (VBG beginning)\n        (S\n          (VP (TO to)\n            (VP (VB get)\n              (ADJP (RB very) (JJ tired))\n              (PP (IN of)\n                (S\n                  (VP (VBG sitting)\n                    (PP (IN by)\n                      (NP\n                        (NP (PRP$ her) (NN sister))\n                        (PP (IN on)\n                          (NP (DT the) (NN bank)))))))))))))\n    (. .)))\n\n",
--                            stoks =
--                              [Token0{tid = TokenID0{untid0 = 1},
--                                      tword = Wordform0{word0 = "Alice"},
--                                      tlemma = Lemma0{lemma0 = "Alice"}, tbegin = 0, tend = 5,
--                                      tpos = NNP, tpostt = "", tner = ["PERSON"],
--                                      tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 2},
--                                      tword = Wordform0{word0 = "was"},
--                                      tlemma = Lemma0{lemma0 = "be"}, tbegin = 6, tend = 9,
--                                      tpos = VBD, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 3},
--                                      tword = Wordform0{word0 = "beginning"},
--                                      tlemma = Lemma0{lemma0 = "begin"}, tbegin = 10, tend = 19,
--                                      tpos = VBG, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 4}, tword = Wordform0{word0 = "to"},
--                                      tlemma = Lemma0{lemma0 = "to"}, tbegin = 20, tend = 22,
--                                      tpos = TO, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 5},
--                                      tword = Wordform0{word0 = "get"},
--                                      tlemma = Lemma0{lemma0 = "get"}, tbegin = 23, tend = 26,
--                                      tpos = VB, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 6},
--                                      tword = Wordform0{word0 = "very"},
--                                      tlemma = Lemma0{lemma0 = "very"}, tbegin = 27, tend = 31,
--                                      tpos = RB, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 7},
--                                      tword = Wordform0{word0 = "tired"},
--                                      tlemma = Lemma0{lemma0 = "tired"}, tbegin = 32, tend = 37,
--                                      tpos = JJ, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 8}, tword = Wordform0{word0 = "of"},
--                                      tlemma = Lemma0{lemma0 = "of"}, tbegin = 38, tend = 40,
--                                      tpos = IN, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 9},
--                                      tword = Wordform0{word0 = "sitting"},
--                                      tlemma = Lemma0{lemma0 = "sit"}, tbegin = 41, tend = 48,
--                                      tpos = VBG, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 10},
--                                      tword = Wordform0{word0 = "by"},
--                                      tlemma = Lemma0{lemma0 = "by"}, tbegin = 49, tend = 51,
--                                      tpos = IN, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 11},
--                                      tword = Wordform0{word0 = "her"},
--                                      tlemma = Lemma0{lemma0 = "she"}, tbegin = 52, tend = 55,
--                                      tpos = PRPdollar, tpostt = "", tner = ["O"],
--                                      tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 12},
--                                      tword = Wordform0{word0 = "sister"},
--                                      tlemma = Lemma0{lemma0 = "sister"}, tbegin = 56, tend = 62,
--                                      tpos = NN, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 13},
--                                      tword = Wordform0{word0 = "on"},
--                                      tlemma = Lemma0{lemma0 = "on"}, tbegin = 63, tend = 65,
--                                      tpos = IN, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 14},
--                                      tword = Wordform0{word0 = "the"},
--                                      tlemma = Lemma0{lemma0 = "the"}, tbegin = 66, tend = 69,
--                                      tpos = DT, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 15},
--                                      tword = Wordform0{word0 = "bank"},
--                                      tlemma = Lemma0{lemma0 = "bank"}, tbegin = 70, tend = 74,
--                                      tpos = NN, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 16}, tword = Wordform0{word0 = "."},
--                                      tlemma = Lemma0{lemma0 = "."}, tbegin = 74, tend = 75,
--                                      tpos = Term, tpostt = "", tner = ["O"], tspeaker = [PER0]}],
--                            sdeps =
--                              Just
--                                (DependenceType0{dtt = "enhanced-plus-plus-dependencies",
--                                                 dtd =
--                                                   [Dependence0{dtype =
--                                                                  DepCode{d1 = ROOT, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 0},
--                                                                      dword =
--                                                                        Wordform0{word0 = "ROOT"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 3},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "beginning"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = NSUBJ, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 3},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "beginning"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 1},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "Alice"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = NSUBJ,
--                                                                          d2 = Missing "XSUBJ"},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 5},
--                                                                      dword =
--                                                                        Wordform0{word0 = "get"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 1},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "Alice"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = AUX, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 3},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "beginning"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 2},
--                                                                      dword =
--                                                                        Wordform0{word0 = "was"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = MARK, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 5},
--                                                                      dword =
--                                                                        Wordform0{word0 = "get"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 4},
--                                                                      dword =
--                                                                        Wordform0{word0 = "to"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = XCOMP, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 3},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "beginning"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 5},
--                                                                      dword =
--                                                                        Wordform0{word0 = "get"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = ADVMOD,
--                                                                          d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 7},
--                                                                      dword =
--                                                                        Wordform0{word0 = "tired"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 6},
--                                                                      dword =
--                                                                        Wordform0{word0 = "very"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = XCOMP, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 5},
--                                                                      dword =
--                                                                        Wordform0{word0 = "get"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 7},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "tired"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = MARK, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 9},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "sitting"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 8},
--                                                                      dword =
--                                                                        Wordform0{word0 = "of"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = ADVCL, d2 = OF},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 5},
--                                                                      dword =
--                                                                        Wordform0{word0 = "get"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 9},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "sitting"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = CASE, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 12},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "sister"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 10},
--                                                                      dword =
--                                                                        Wordform0{word0 = "by"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = NMOD, d2 = POSS},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 12},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "sister"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 11},
--                                                                      dword =
--                                                                        Wordform0{word0 = "her"}}},
--                                                    Dependence0{dtype = DepCode{d1 = NMOD, d2 = BY},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 9},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "sitting"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 12},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "sister"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = CASE, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 15},
--                                                                      dword =
--                                                                        Wordform0{word0 = "bank"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 13},
--                                                                      dword =
--                                                                        Wordform0{word0 = "on"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = DET, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 15},
--                                                                      dword =
--                                                                        Wordform0{word0 = "bank"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 14},
--                                                                      dword =
--                                                                        Wordform0{word0 = "the"}}},
--                                                    Dependence0{dtype = DepCode{d1 = NMOD, d2 = ON},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 12},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "sister"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 15},
--                                                                      dword =
--                                                                        Wordform0{word0 = "bank"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = PUNCT, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 3},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "beginning"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 16},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "."}}}]})}],
--               docCorefs =
--                 [Coref0{corefMents =
--                           [Mention0{mentRep = "true", mentSent = SentID0{unSentID0 = 1},
--                                     mentStart = TokenID0{untid0 = 1},
--                                     mentEnd = TokenID0{untid0 = 2},
--                                     mentHead = TokenID0{untid0 = 1}, mentText = "Alice"},
--                            Mention0{mentRep = "", mentSent = SentID0{unSentID0 = 1},
--                                     mentStart = TokenID0{untid0 = 11},
--                                     mentEnd = TokenID0{untid0 = 12},
--                                     mentHead = TokenID0{untid0 = 11}, mentText = "her"}]}]}),
--       Just
--         (NLPtext{tz3loc = TextLoc{tlpage = "", tlline = 39},
--                  tz3text = "Ein deutscher Satz.", tz3lang = German},
--          Doc0{docSents =
--                 [Sentence0{sid = SentID0{unSentID0 = 1},
--                            sparse =
--                              "(ROOT\n  (NP (ART Ein) (ADJA deutscher) (NN Satz) ($. .)))\n\n",
--                            stoks =
--                              [Token0{tid = TokenID0{untid0 = 1},
--                                      tword = Wordform0{word0 = "Ein"},
--                                      tlemma = Lemma0{lemma0 = "ein"}, tbegin = 0, tend = 3,
--                                      tpos = Unk, tpostt = "", tner = ["O"], tspeaker = []},
--                               Token0{tid = TokenID0{untid0 = 2},
--                                      tword = Wordform0{word0 = "deutscher"},
--                                      tlemma = Lemma0{lemma0 = "deutscher"}, tbegin = 4, tend = 13,
--                                      tpos = Unk, tpostt = "", tner = ["I-MISC"], tspeaker = []},
--                               Token0{tid = TokenID0{untid0 = 3},
--                                      tword = Wordform0{word0 = "Satz"},
--                                      tlemma = Lemma0{lemma0 = "satz"}, tbegin = 14, tend = 18,
--                                      tpos = NN, tpostt = "", tner = ["O"], tspeaker = []},
--                               Token0{tid = TokenID0{untid0 = 4}, tword = Wordform0{word0 = "."},
--                                      tlemma = Lemma0{lemma0 = "."}, tbegin = 18, tend = 19,
--                                      tpos = Unk, tpostt = "", tner = ["O"], tspeaker = []}],
--                            sdeps = Nothing}],
--               docCorefs = []}),
--       Just
--         (NLPtext{tz3loc = TextLoc{tlpage = "", tlline = 43},
--                  tz3text = "There was nothing so VERY remarkable in that.",
--                  tz3lang = English},
--          Doc0{docSents =
--                 [Sentence0{sid = SentID0{unSentID0 = 1},
--                            sparse =
--                              "(ROOT\n  (S\n    (NP (EX There))\n    (VP (VBD was)\n      (ADVP (NN nothing))\n      (ADJP\n        (ADJP (RB so) (RB VERY) (JJ remarkable))\n        (PP (IN in)\n          (NP (DT that)))))\n    (. .)))\n\n",
--                            stoks =
--                              [Token0{tid = TokenID0{untid0 = 1},
--                                      tword = Wordform0{word0 = "There"},
--                                      tlemma = Lemma0{lemma0 = "there"}, tbegin = 0, tend = 5,
--                                      tpos = EX, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 2},
--                                      tword = Wordform0{word0 = "was"},
--                                      tlemma = Lemma0{lemma0 = "be"}, tbegin = 6, tend = 9,
--                                      tpos = VBD, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 3},
--                                      tword = Wordform0{word0 = "nothing"},
--                                      tlemma = Lemma0{lemma0 = "nothing"}, tbegin = 10, tend = 17,
--                                      tpos = NN, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 4}, tword = Wordform0{word0 = "so"},
--                                      tlemma = Lemma0{lemma0 = "so"}, tbegin = 18, tend = 20,
--                                      tpos = RB, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 5},
--                                      tword = Wordform0{word0 = "VERY"},
--                                      tlemma = Lemma0{lemma0 = "very"}, tbegin = 21, tend = 25,
--                                      tpos = RB, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 6},
--                                      tword = Wordform0{word0 = "remarkable"},
--                                      tlemma = Lemma0{lemma0 = "remarkable"}, tbegin = 26,
--                                      tend = 36, tpos = JJ, tpostt = "", tner = ["O"],
--                                      tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 7}, tword = Wordform0{word0 = "in"},
--                                      tlemma = Lemma0{lemma0 = "in"}, tbegin = 37, tend = 39,
--                                      tpos = IN, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 8},
--                                      tword = Wordform0{word0 = "that"},
--                                      tlemma = Lemma0{lemma0 = "that"}, tbegin = 40, tend = 44,
--                                      tpos = DT, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 9}, tword = Wordform0{word0 = "."},
--                                      tlemma = Lemma0{lemma0 = "."}, tbegin = 44, tend = 45,
--                                      tpos = Term, tpostt = "", tner = ["O"], tspeaker = [PER0]}],
--                            sdeps =
--                              Just
--                                (DependenceType0{dtt = "enhanced-plus-plus-dependencies",
--                                                 dtd =
--                                                   [Dependence0{dtype =
--                                                                  DepCode{d1 = ROOT, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 0},
--                                                                      dword =
--                                                                        Wordform0{word0 = "ROOT"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 2},
--                                                                      dword =
--                                                                        Wordform0{word0 = "was"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = EXPL, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 2},
--                                                                      dword =
--                                                                        Wordform0{word0 = "was"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 1},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "There"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = ADVMOD,
--                                                                          d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 2},
--                                                                      dword =
--                                                                        Wordform0{word0 = "was"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 3},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "nothing"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = ADVMOD,
--                                                                          d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 6},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "remarkable"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 4},
--                                                                      dword =
--                                                                        Wordform0{word0 = "so"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = ADVMOD,
--                                                                          d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 6},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "remarkable"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 5},
--                                                                      dword =
--                                                                        Wordform0{word0 = "VERY"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = XCOMP, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 2},
--                                                                      dword =
--                                                                        Wordform0{word0 = "was"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 6},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "remarkable"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = CASE, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 8},
--                                                                      dword =
--                                                                        Wordform0{word0 = "that"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 7},
--                                                                      dword =
--                                                                        Wordform0{word0 = "in"}}},
--                                                    Dependence0{dtype = DepCode{d1 = NMOD, d2 = IN},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 6},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "remarkable"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 8},
--                                                                      dword =
--                                                                        Wordform0{word0 = "that"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = PUNCT, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 2},
--                                                                      dword =
--                                                                        Wordform0{word0 = "was"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 9},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "."}}}]})}],
--               docCorefs = []}),
--       Just
--         (NLPtext{tz3loc = TextLoc{tlpage = "", tlline = 45},
--                  tz3text =
--                    "In another moment down went Alice after it, never once considering how in the world she was to get out again.",
--                  tz3lang = English},
--          Doc0{docSents =
--                 [Sentence0{sid = SentID0{unSentID0 = 1},
--                            sparse =
--                              "(ROOT\n  (SINV\n    (PP (IN In)\n      (ADVP\n        (NP (DT another) (NN moment))\n        (IN down)))\n    (VP (VBD went))\n    (NP\n      (NP (NNP Alice))\n      (PP (IN after)\n        (NP (PRP it))))\n    (, ,)\n    (S\n      (VP\n        (ADVP (RB never) (RB once))\n        (VBG considering)\n        (SBAR\n          (WHADVP (WRB how))\n          (S\n            (PP (IN in)\n              (NP (DT the) (NN world)))\n            (NP (PRP she))\n            (VP (VBD was)\n              (S\n                (VP (TO to)\n                  (VP (VB get)\n                    (PRT (RP out))\n                    (ADVP (RB again))))))))))\n    (. .)))\n\n",
--                            stoks =
--                              [Token0{tid = TokenID0{untid0 = 1},
--                                      tword = Wordform0{word0 = "In"},
--                                      tlemma = Lemma0{lemma0 = "in"}, tbegin = 0, tend = 2,
--                                      tpos = IN, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 2},
--                                      tword = Wordform0{word0 = "another"},
--                                      tlemma = Lemma0{lemma0 = "another"}, tbegin = 3, tend = 10,
--                                      tpos = DT, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 3},
--                                      tword = Wordform0{word0 = "moment"},
--                                      tlemma = Lemma0{lemma0 = "moment"}, tbegin = 11, tend = 17,
--                                      tpos = NN, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 4},
--                                      tword = Wordform0{word0 = "down"},
--                                      tlemma = Lemma0{lemma0 = "down"}, tbegin = 18, tend = 22,
--                                      tpos = IN, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 5},
--                                      tword = Wordform0{word0 = "went"},
--                                      tlemma = Lemma0{lemma0 = "go"}, tbegin = 23, tend = 27,
--                                      tpos = VBD, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 6},
--                                      tword = Wordform0{word0 = "Alice"},
--                                      tlemma = Lemma0{lemma0 = "Alice"}, tbegin = 28, tend = 33,
--                                      tpos = NNP, tpostt = "", tner = ["PERSON"],
--                                      tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 7},
--                                      tword = Wordform0{word0 = "after"},
--                                      tlemma = Lemma0{lemma0 = "after"}, tbegin = 34, tend = 39,
--                                      tpos = IN, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 8}, tword = Wordform0{word0 = "it"},
--                                      tlemma = Lemma0{lemma0 = "it"}, tbegin = 40, tend = 42,
--                                      tpos = PRP, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 9}, tword = Wordform0{word0 = ","},
--                                      tlemma = Lemma0{lemma0 = ","}, tbegin = 42, tend = 43,
--                                      tpos = Comma, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 10},
--                                      tword = Wordform0{word0 = "never"},
--                                      tlemma = Lemma0{lemma0 = "never"}, tbegin = 44, tend = 49,
--                                      tpos = RB, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 11},
--                                      tword = Wordform0{word0 = "once"},
--                                      tlemma = Lemma0{lemma0 = "once"}, tbegin = 50, tend = 54,
--                                      tpos = RB, tpostt = "", tner = ["DATE", "PAST_REF"],
--                                      tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 12},
--                                      tword = Wordform0{word0 = "considering"},
--                                      tlemma = Lemma0{lemma0 = "consider"}, tbegin = 55, tend = 66,
--                                      tpos = VBG, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 13},
--                                      tword = Wordform0{word0 = "how"},
--                                      tlemma = Lemma0{lemma0 = "how"}, tbegin = 67, tend = 70,
--                                      tpos = WRB, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 14},
--                                      tword = Wordform0{word0 = "in"},
--                                      tlemma = Lemma0{lemma0 = "in"}, tbegin = 71, tend = 73,
--                                      tpos = IN, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 15},
--                                      tword = Wordform0{word0 = "the"},
--                                      tlemma = Lemma0{lemma0 = "the"}, tbegin = 74, tend = 77,
--                                      tpos = DT, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 16},
--                                      tword = Wordform0{word0 = "world"},
--                                      tlemma = Lemma0{lemma0 = "world"}, tbegin = 78, tend = 83,
--                                      tpos = NN, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 17},
--                                      tword = Wordform0{word0 = "she"},
--                                      tlemma = Lemma0{lemma0 = "she"}, tbegin = 84, tend = 87,
--                                      tpos = PRP, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 18},
--                                      tword = Wordform0{word0 = "was"},
--                                      tlemma = Lemma0{lemma0 = "be"}, tbegin = 88, tend = 91,
--                                      tpos = VBD, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 19},
--                                      tword = Wordform0{word0 = "to"},
--                                      tlemma = Lemma0{lemma0 = "to"}, tbegin = 92, tend = 94,
--                                      tpos = TO, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 20},
--                                      tword = Wordform0{word0 = "get"},
--                                      tlemma = Lemma0{lemma0 = "get"}, tbegin = 95, tend = 98,
--                                      tpos = VB, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 21},
--                                      tword = Wordform0{word0 = "out"},
--                                      tlemma = Lemma0{lemma0 = "out"}, tbegin = 99, tend = 102,
--                                      tpos = RP, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 22},
--                                      tword = Wordform0{word0 = "again"},
--                                      tlemma = Lemma0{lemma0 = "again"}, tbegin = 103, tend = 108,
--                                      tpos = RB, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 23}, tword = Wordform0{word0 = "."},
--                                      tlemma = Lemma0{lemma0 = "."}, tbegin = 108, tend = 109,
--                                      tpos = Term, tpostt = "", tner = ["O"], tspeaker = [PER0]}],
--                            sdeps =
--                              Just
--                                (DependenceType0{dtt = "enhanced-plus-plus-dependencies",
--                                                 dtd =
--                                                   [Dependence0{dtype =
--                                                                  DepCode{d1 = ROOT, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 0},
--                                                                      dword =
--                                                                        Wordform0{word0 = "ROOT"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 5},
--                                                                      dword =
--                                                                        Wordform0{word0 = "went"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = CASE, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 3},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "moment"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 1},
--                                                                      dword =
--                                                                        Wordform0{word0 = "In"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = DET, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 3},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "moment"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 2},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "another"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = NMOD, d2 = DOWN},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 5},
--                                                                      dword =
--                                                                        Wordform0{word0 = "went"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 3},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "moment"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = CASE, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 3},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "moment"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 4},
--                                                                      dword =
--                                                                        Wordform0{word0 = "down"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = NSUBJ, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 5},
--                                                                      dword =
--                                                                        Wordform0{word0 = "went"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 6},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "Alice"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = CASE, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 8},
--                                                                      dword =
--                                                                        Wordform0{word0 = "it"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 7},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "after"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = NMOD, d2 = AFTER},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 6},
--                                                                      dword =
--                                                                        Wordform0{word0 = "Alice"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 8},
--                                                                      dword =
--                                                                        Wordform0{word0 = "it"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = PUNCT, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 5},
--                                                                      dword =
--                                                                        Wordform0{word0 = "went"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 9},
--                                                                      dword =
--                                                                        Wordform0{word0 = ","}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = DEP, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 11},
--                                                                      dword =
--                                                                        Wordform0{word0 = "once"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 10},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "never"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = ADVMOD,
--                                                                          d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 12},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "considering"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 11},
--                                                                      dword =
--                                                                        Wordform0{word0 = "once"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = ADVCL, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 5},
--                                                                      dword =
--                                                                        Wordform0{word0 = "went"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 12},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "considering"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = ADVMOD,
--                                                                          d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 18},
--                                                                      dword =
--                                                                        Wordform0{word0 = "was"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 13},
--                                                                      dword =
--                                                                        Wordform0{word0 = "how"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = CASE, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 16},
--                                                                      dword =
--                                                                        Wordform0{word0 = "world"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 14},
--                                                                      dword =
--                                                                        Wordform0{word0 = "in"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = DET, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 16},
--                                                                      dword =
--                                                                        Wordform0{word0 = "world"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 15},
--                                                                      dword =
--                                                                        Wordform0{word0 = "the"}}},
--                                                    Dependence0{dtype = DepCode{d1 = NMOD, d2 = IN},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 18},
--                                                                      dword =
--                                                                        Wordform0{word0 = "was"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 16},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "world"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = NSUBJ, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 18},
--                                                                      dword =
--                                                                        Wordform0{word0 = "was"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 17},
--                                                                      dword =
--                                                                        Wordform0{word0 = "she"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = NSUBJ,
--                                                                          d2 = Missing "XSUBJ"},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 20},
--                                                                      dword =
--                                                                        Wordform0{word0 = "get"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 17},
--                                                                      dword =
--                                                                        Wordform0{word0 = "she"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = CCOMP, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 12},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "considering"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 18},
--                                                                      dword =
--                                                                        Wordform0{word0 = "was"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = MARK, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 20},
--                                                                      dword =
--                                                                        Wordform0{word0 = "get"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 19},
--                                                                      dword =
--                                                                        Wordform0{word0 = "to"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = XCOMP, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 18},
--                                                                      dword =
--                                                                        Wordform0{word0 = "was"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 20},
--                                                                      dword =
--                                                                        Wordform0{word0 = "get"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = COMPOUND, d2 = PRT},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 20},
--                                                                      dword =
--                                                                        Wordform0{word0 = "get"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 21},
--                                                                      dword =
--                                                                        Wordform0{word0 = "out"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = ADVMOD,
--                                                                          d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 20},
--                                                                      dword =
--                                                                        Wordform0{word0 = "get"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 22},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "again"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = PUNCT, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 5},
--                                                                      dword =
--                                                                        Wordform0{word0 = "went"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 23},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "."}}}]})}],
--               docCorefs =
--                 [Coref0{corefMents =
--                           [Mention0{mentRep = "true", mentSent = SentID0{unSentID0 = 1},
--                                     mentStart = TokenID0{untid0 = 2},
--                                     mentEnd = TokenID0{untid0 = 4},
--                                     mentHead = TokenID0{untid0 = 3}, mentText = "another moment"},
--                            Mention0{mentRep = "", mentSent = SentID0{unSentID0 = 1},
--                                     mentStart = TokenID0{untid0 = 8},
--                                     mentEnd = TokenID0{untid0 = 9},
--                                     mentHead = TokenID0{untid0 = 8}, mentText = "it"},
--                            Mention0{mentRep = "true", mentSent = SentID0{unSentID0 = 1},
--                                     mentStart = TokenID0{untid0 = 6},
--                                     mentEnd = TokenID0{untid0 = 9},
--                                     mentHead = TokenID0{untid0 = 6}, mentText = "Alice after it"},
--                            Mention0{mentRep = "", mentSent = SentID0{unSentID0 = 1},
--                                     mentStart = TokenID0{untid0 = 17},
--                                     mentEnd = TokenID0{untid0 = 18},
--                                     mentHead = TokenID0{untid0 = 17}, mentText = "she"}]}]}),
--       Just
--         (NLPtext{tz3loc = TextLoc{tlpage = "", tlline = 49},
--                  tz3text = " 'Fury said to a mouse, That he met in the house.",
--                  tz3lang = English},
--          Doc0{docSents =
--                 [Sentence0{sid = SentID0{unSentID0 = 1},
--                            sparse =
--                              "(ROOT\n  (S (`` `)\n    (NP (NN Fury))\n    (VP (VBD said)\n      (PP (TO to)\n        (NP (DT a) (NN mouse)))\n      (, ,)\n      (SBAR (IN That)\n        (S\n          (NP (PRP he))\n          (VP (VBD met)\n            (PP (IN in)\n              (NP (DT the) (NN house)))))))\n    (. .)))\n\n",
--                            stoks =
--                              [Token0{tid = TokenID0{untid0 = 1}, tword = Wordform0{word0 = "`"},
--                                      tlemma = Lemma0{lemma0 = "`"}, tbegin = 0, tend = 1,
--                                      tpos = OpenDQuote, tpostt = "", tner = ["O"],
--                                      tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 2},
--                                      tword = Wordform0{word0 = "Fury"},
--                                      tlemma = Lemma0{lemma0 = "fury"}, tbegin = 1, tend = 5,
--                                      tpos = NN, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 3},
--                                      tword = Wordform0{word0 = "said"},
--                                      tlemma = Lemma0{lemma0 = "say"}, tbegin = 6, tend = 10,
--                                      tpos = VBD, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 4}, tword = Wordform0{word0 = "to"},
--                                      tlemma = Lemma0{lemma0 = "to"}, tbegin = 11, tend = 13,
--                                      tpos = TO, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 5}, tword = Wordform0{word0 = "a"},
--                                      tlemma = Lemma0{lemma0 = "a"}, tbegin = 14, tend = 15,
--                                      tpos = DT, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 6},
--                                      tword = Wordform0{word0 = "mouse"},
--                                      tlemma = Lemma0{lemma0 = "mouse"}, tbegin = 16, tend = 21,
--                                      tpos = NN, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 7}, tword = Wordform0{word0 = ","},
--                                      tlemma = Lemma0{lemma0 = ","}, tbegin = 21, tend = 22,
--                                      tpos = Comma, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 8},
--                                      tword = Wordform0{word0 = "That"},
--                                      tlemma = Lemma0{lemma0 = "that"}, tbegin = 23, tend = 27,
--                                      tpos = IN, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 9}, tword = Wordform0{word0 = "he"},
--                                      tlemma = Lemma0{lemma0 = "he"}, tbegin = 28, tend = 30,
--                                      tpos = PRP, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 10},
--                                      tword = Wordform0{word0 = "met"},
--                                      tlemma = Lemma0{lemma0 = "meet"}, tbegin = 31, tend = 34,
--                                      tpos = VBD, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 11},
--                                      tword = Wordform0{word0 = "in"},
--                                      tlemma = Lemma0{lemma0 = "in"}, tbegin = 35, tend = 37,
--                                      tpos = IN, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 12},
--                                      tword = Wordform0{word0 = "the"},
--                                      tlemma = Lemma0{lemma0 = "the"}, tbegin = 38, tend = 41,
--                                      tpos = DT, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 13},
--                                      tword = Wordform0{word0 = "house"},
--                                      tlemma = Lemma0{lemma0 = "house"}, tbegin = 42, tend = 47,
--                                      tpos = NN, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 14}, tword = Wordform0{word0 = "."},
--                                      tlemma = Lemma0{lemma0 = "."}, tbegin = 47, tend = 48,
--                                      tpos = Term, tpostt = "", tner = ["O"], tspeaker = [PER0]}],
--                            sdeps =
--                              Just
--                                (DependenceType0{dtt = "enhanced-plus-plus-dependencies",
--                                                 dtd =
--                                                   [Dependence0{dtype =
--                                                                  DepCode{d1 = ROOT, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 0},
--                                                                      dword =
--                                                                        Wordform0{word0 = "ROOT"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 3},
--                                                                      dword =
--                                                                        Wordform0{word0 = "said"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = PUNCT, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 3},
--                                                                      dword =
--                                                                        Wordform0{word0 = "said"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 1},
--                                                                      dword =
--                                                                        Wordform0{word0 = "`"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = NSUBJ, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 3},
--                                                                      dword =
--                                                                        Wordform0{word0 = "said"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 2},
--                                                                      dword =
--                                                                        Wordform0{word0 = "Fury"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = CASE, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 6},
--                                                                      dword =
--                                                                        Wordform0{word0 = "mouse"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 4},
--                                                                      dword =
--                                                                        Wordform0{word0 = "to"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = DET, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 6},
--                                                                      dword =
--                                                                        Wordform0{word0 = "mouse"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 5},
--                                                                      dword =
--                                                                        Wordform0{word0 = "a"}}},
--                                                    Dependence0{dtype = DepCode{d1 = NMOD, d2 = TO},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 3},
--                                                                      dword =
--                                                                        Wordform0{word0 = "said"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 6},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "mouse"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = PUNCT, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 3},
--                                                                      dword =
--                                                                        Wordform0{word0 = "said"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 7},
--                                                                      dword =
--                                                                        Wordform0{word0 = ","}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = MARK, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 10},
--                                                                      dword =
--                                                                        Wordform0{word0 = "met"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 8},
--                                                                      dword =
--                                                                        Wordform0{word0 = "That"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = NSUBJ, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 10},
--                                                                      dword =
--                                                                        Wordform0{word0 = "met"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 9},
--                                                                      dword =
--                                                                        Wordform0{word0 = "he"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = CCOMP, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 3},
--                                                                      dword =
--                                                                        Wordform0{word0 = "said"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 10},
--                                                                      dword =
--                                                                        Wordform0{word0 = "met"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = CASE, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 13},
--                                                                      dword =
--                                                                        Wordform0{word0 = "house"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 11},
--                                                                      dword =
--                                                                        Wordform0{word0 = "in"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = DET, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 13},
--                                                                      dword =
--                                                                        Wordform0{word0 = "house"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 12},
--                                                                      dword =
--                                                                        Wordform0{word0 = "the"}}},
--                                                    Dependence0{dtype = DepCode{d1 = NMOD, d2 = IN},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 10},
--                                                                      dword =
--                                                                        Wordform0{word0 = "met"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 13},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "house"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = PUNCT, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 3},
--                                                                      dword =
--                                                                        Wordform0{word0 = "said"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 14},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "."}}}]})}],
--               docCorefs = []}),
--       Just
--         (NLPtext{tz3loc = TextLoc{tlpage = "", tlline = 55},
--                  tz3text = "CHAPTER IV. The Rabbit Sends in a Little Bill",
--                  tz3lang = English},
--          Doc0{docSents =
--                 [Sentence0{sid = SentID0{unSentID0 = 1},
--                            sparse =
--                              "(ROOT\n  (NP\n    (NP (NN CHAPTER))\n    (NP (CD IV))\n    (. .)))\n\n",
--                            stoks =
--                              [Token0{tid = TokenID0{untid0 = 1},
--                                      tword = Wordform0{word0 = "CHAPTER"},
--                                      tlemma = Lemma0{lemma0 = "chapter"}, tbegin = 0, tend = 7,
--                                      tpos = NN, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 2}, tword = Wordform0{word0 = "IV"},
--                                      tlemma = Lemma0{lemma0 = "iv"}, tbegin = 8, tend = 10,
--                                      tpos = CD, tpostt = "", tner = ["NUMBER", "0.0"],
--                                      tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 3}, tword = Wordform0{word0 = "."},
--                                      tlemma = Lemma0{lemma0 = "."}, tbegin = 10, tend = 11,
--                                      tpos = Term, tpostt = "", tner = ["O"], tspeaker = [PER0]}],
--                            sdeps =
--                              Just
--                                (DependenceType0{dtt = "enhanced-plus-plus-dependencies",
--                                                 dtd =
--                                                   [Dependence0{dtype =
--                                                                  DepCode{d1 = ROOT, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 0},
--                                                                      dword =
--                                                                        Wordform0{word0 = "ROOT"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 1},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "CHAPTER"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = DEP, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 1},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "CHAPTER"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 2},
--                                                                      dword =
--                                                                        Wordform0{word0 = "IV"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = PUNCT, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 1},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "CHAPTER"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 3},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "."}}}]})},
--                  Sentence0{sid = SentID0{unSentID0 = 2},
--                            sparse =
--                              "(ROOT\n  (S\n    (NP (DT The) (NNP Rabbit))\n    (VP (VBZ Sends)\n      (PP (IN in)\n        (NP (DT a) (JJ Little) (NN Bill))))))\n\n",
--                            stoks =
--                              [Token0{tid = TokenID0{untid0 = 1},
--                                      tword = Wordform0{word0 = "The"},
--                                      tlemma = Lemma0{lemma0 = "the"}, tbegin = 12, tend = 15,
--                                      tpos = DT, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 2},
--                                      tword = Wordform0{word0 = "Rabbit"},
--                                      tlemma = Lemma0{lemma0 = "Rabbit"}, tbegin = 16, tend = 22,
--                                      tpos = NNP, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 3},
--                                      tword = Wordform0{word0 = "Sends"},
--                                      tlemma = Lemma0{lemma0 = "send"}, tbegin = 23, tend = 28,
--                                      tpos = VBZ, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 4}, tword = Wordform0{word0 = "in"},
--                                      tlemma = Lemma0{lemma0 = "in"}, tbegin = 29, tend = 31,
--                                      tpos = IN, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 5}, tword = Wordform0{word0 = "a"},
--                                      tlemma = Lemma0{lemma0 = "a"}, tbegin = 32, tend = 33,
--                                      tpos = DT, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 6},
--                                      tword = Wordform0{word0 = "Little"},
--                                      tlemma = Lemma0{lemma0 = "little"}, tbegin = 34, tend = 40,
--                                      tpos = JJ, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 7},
--                                      tword = Wordform0{word0 = "Bill"},
--                                      tlemma = Lemma0{lemma0 = "bill"}, tbegin = 41, tend = 45,
--                                      tpos = NN, tpostt = "", tner = ["O"], tspeaker = [PER0]}],
--                            sdeps =
--                              Just
--                                (DependenceType0{dtt = "enhanced-plus-plus-dependencies",
--                                                 dtd =
--                                                   [Dependence0{dtype =
--                                                                  DepCode{d1 = ROOT, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 0},
--                                                                      dword =
--                                                                        Wordform0{word0 = "ROOT"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 3},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "Sends"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = DET, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 2},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "Rabbit"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 1},
--                                                                      dword =
--                                                                        Wordform0{word0 = "The"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = NSUBJ, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 3},
--                                                                      dword =
--                                                                        Wordform0{word0 = "Sends"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 2},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "Rabbit"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = CASE, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 7},
--                                                                      dword =
--                                                                        Wordform0{word0 = "Bill"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 4},
--                                                                      dword =
--                                                                        Wordform0{word0 = "in"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = DET, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 7},
--                                                                      dword =
--                                                                        Wordform0{word0 = "Bill"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 5},
--                                                                      dword =
--                                                                        Wordform0{word0 = "a"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = AMOD, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 7},
--                                                                      dword =
--                                                                        Wordform0{word0 = "Bill"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 6},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "Little"}}},
--                                                    Dependence0{dtype = DepCode{d1 = NMOD, d2 = IN},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 3},
--                                                                      dword =
--                                                                        Wordform0{word0 = "Sends"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 7},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "Bill"}}}]})}],
--               docCorefs = []}),
--       Just
--         (NLPtext{tz3loc = TextLoc{tlpage = "", tlline = 57},
--                  tz3text =
--                    "It was the White Rabbit, trotting slowly back again, and looking anxiously about as it went, as if it had lost something .",
--                  tz3lang = English},
--          Doc0{docSents =
--                 [Sentence0{sid = SentID0{unSentID0 = 1},
--                            sparse =
--                              "(ROOT\n  (S\n    (NP (PRP It))\n    (VP (VBD was)\n      (NP (DT the) (NNP White) (NNP Rabbit))\n      (, ,)\n      (S\n        (VP\n          (VP (VBG trotting)\n            (ADVP (RB slowly) (RB back))\n            (ADVP (RB again)))\n          (, ,)\n          (CC and)\n          (VP (VBG looking)\n            (ADVP (RB anxiously))\n            (SBAR\n              (ADVP (RB about))\n              (IN as)\n              (S\n                (NP (PRP it))\n                (VP (VBD went) (, ,)\n                  (SBAR (IN as) (IN if)\n                    (S\n                      (NP (PRP it))\n                      (VP (VBD had)\n                        (VP (VBN lost)\n                          (NP (NN something)))))))))))))\n    (. .)))\n\n",
--                            stoks =
--                              [Token0{tid = TokenID0{untid0 = 1},
--                                      tword = Wordform0{word0 = "It"},
--                                      tlemma = Lemma0{lemma0 = "it"}, tbegin = 0, tend = 2,
--                                      tpos = PRP, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 2},
--                                      tword = Wordform0{word0 = "was"},
--                                      tlemma = Lemma0{lemma0 = "be"}, tbegin = 3, tend = 6,
--                                      tpos = VBD, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 3},
--                                      tword = Wordform0{word0 = "the"},
--                                      tlemma = Lemma0{lemma0 = "the"}, tbegin = 7, tend = 10,
--                                      tpos = DT, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 4},
--                                      tword = Wordform0{word0 = "White"},
--                                      tlemma = Lemma0{lemma0 = "White"}, tbegin = 11, tend = 16,
--                                      tpos = NNP, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 5},
--                                      tword = Wordform0{word0 = "Rabbit"},
--                                      tlemma = Lemma0{lemma0 = "Rabbit"}, tbegin = 17, tend = 23,
--                                      tpos = NNP, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 6}, tword = Wordform0{word0 = ","},
--                                      tlemma = Lemma0{lemma0 = ","}, tbegin = 23, tend = 24,
--                                      tpos = Comma, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 7},
--                                      tword = Wordform0{word0 = "trotting"},
--                                      tlemma = Lemma0{lemma0 = "trot"}, tbegin = 25, tend = 33,
--                                      tpos = VBG, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 8},
--                                      tword = Wordform0{word0 = "slowly"},
--                                      tlemma = Lemma0{lemma0 = "slowly"}, tbegin = 34, tend = 40,
--                                      tpos = RB, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 9},
--                                      tword = Wordform0{word0 = "back"},
--                                      tlemma = Lemma0{lemma0 = "back"}, tbegin = 41, tend = 45,
--                                      tpos = RB, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 10},
--                                      tword = Wordform0{word0 = "again"},
--                                      tlemma = Lemma0{lemma0 = "again"}, tbegin = 46, tend = 51,
--                                      tpos = RB, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 11}, tword = Wordform0{word0 = ","},
--                                      tlemma = Lemma0{lemma0 = ","}, tbegin = 51, tend = 52,
--                                      tpos = Comma, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 12},
--                                      tword = Wordform0{word0 = "and"},
--                                      tlemma = Lemma0{lemma0 = "and"}, tbegin = 53, tend = 56,
--                                      tpos = CC, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 13},
--                                      tword = Wordform0{word0 = "looking"},
--                                      tlemma = Lemma0{lemma0 = "look"}, tbegin = 57, tend = 64,
--                                      tpos = VBG, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 14},
--                                      tword = Wordform0{word0 = "anxiously"},
--                                      tlemma = Lemma0{lemma0 = "anxiously"}, tbegin = 65, tend = 74,
--                                      tpos = RB, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 15},
--                                      tword = Wordform0{word0 = "about"},
--                                      tlemma = Lemma0{lemma0 = "about"}, tbegin = 75, tend = 80,
--                                      tpos = RB, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 16},
--                                      tword = Wordform0{word0 = "as"},
--                                      tlemma = Lemma0{lemma0 = "as"}, tbegin = 81, tend = 83,
--                                      tpos = IN, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 17},
--                                      tword = Wordform0{word0 = "it"},
--                                      tlemma = Lemma0{lemma0 = "it"}, tbegin = 84, tend = 86,
--                                      tpos = PRP, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 18},
--                                      tword = Wordform0{word0 = "went"},
--                                      tlemma = Lemma0{lemma0 = "go"}, tbegin = 87, tend = 91,
--                                      tpos = VBD, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 19}, tword = Wordform0{word0 = ","},
--                                      tlemma = Lemma0{lemma0 = ","}, tbegin = 91, tend = 92,
--                                      tpos = Comma, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 20},
--                                      tword = Wordform0{word0 = "as"},
--                                      tlemma = Lemma0{lemma0 = "as"}, tbegin = 93, tend = 95,
--                                      tpos = IN, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 21},
--                                      tword = Wordform0{word0 = "if"},
--                                      tlemma = Lemma0{lemma0 = "if"}, tbegin = 96, tend = 98,
--                                      tpos = IN, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 22},
--                                      tword = Wordform0{word0 = "it"},
--                                      tlemma = Lemma0{lemma0 = "it"}, tbegin = 99, tend = 101,
--                                      tpos = PRP, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 23},
--                                      tword = Wordform0{word0 = "had"},
--                                      tlemma = Lemma0{lemma0 = "have"}, tbegin = 102, tend = 105,
--                                      tpos = VBD, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 24},
--                                      tword = Wordform0{word0 = "lost"},
--                                      tlemma = Lemma0{lemma0 = "lose"}, tbegin = 106, tend = 110,
--                                      tpos = VBN, tpostt = "", tner = ["O"], tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 25},
--                                      tword = Wordform0{word0 = "something"},
--                                      tlemma = Lemma0{lemma0 = "something"}, tbegin = 111,
--                                      tend = 120, tpos = NN, tpostt = "", tner = ["O"],
--                                      tspeaker = [PER0]},
--                               Token0{tid = TokenID0{untid0 = 26}, tword = Wordform0{word0 = "."},
--                                      tlemma = Lemma0{lemma0 = "."}, tbegin = 121, tend = 122,
--                                      tpos = Term, tpostt = "", tner = ["O"], tspeaker = [PER0]}],
--                            sdeps =
--                              Just
--                                (DependenceType0{dtt = "enhanced-plus-plus-dependencies",
--                                                 dtd =
--                                                   [Dependence0{dtype =
--                                                                  DepCode{d1 = ROOT, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 0},
--                                                                      dword =
--                                                                        Wordform0{word0 = "ROOT"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 5},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "Rabbit"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = NSUBJ, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 5},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "Rabbit"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 1},
--                                                                      dword =
--                                                                        Wordform0{word0 = "It"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = COP, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 5},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "Rabbit"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 2},
--                                                                      dword =
--                                                                        Wordform0{word0 = "was"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = DET, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 5},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "Rabbit"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 3},
--                                                                      dword =
--                                                                        Wordform0{word0 = "the"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = COMPOUND,
--                                                                          d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 5},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "Rabbit"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 4},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "White"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = PUNCT, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 5},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "Rabbit"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 6},
--                                                                      dword =
--                                                                        Wordform0{word0 = ","}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = DEP, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 5},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "Rabbit"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 7},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "trotting"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = ADVMOD,
--                                                                          d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 9},
--                                                                      dword =
--                                                                        Wordform0{word0 = "back"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 8},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "slowly"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = ADVMOD,
--                                                                          d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 7},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "trotting"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 9},
--                                                                      dword =
--                                                                        Wordform0{word0 = "back"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = ADVMOD,
--                                                                          d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 7},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "trotting"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 10},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "again"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = PUNCT, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 7},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "trotting"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 11},
--                                                                      dword =
--                                                                        Wordform0{word0 = ","}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = CC, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 7},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "trotting"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 12},
--                                                                      dword =
--                                                                        Wordform0{word0 = "and"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = DEP, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 5},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "Rabbit"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 13},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "looking"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = CONJ, d2 = AND},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 7},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "trotting"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 13},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "looking"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = ADVMOD,
--                                                                          d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 13},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "looking"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 14},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "anxiously"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = ADVMOD,
--                                                                          d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 18},
--                                                                      dword =
--                                                                        Wordform0{word0 = "went"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 15},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "about"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = MARK, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 18},
--                                                                      dword =
--                                                                        Wordform0{word0 = "went"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 16},
--                                                                      dword =
--                                                                        Wordform0{word0 = "as"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = NSUBJ, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 18},
--                                                                      dword =
--                                                                        Wordform0{word0 = "went"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 17},
--                                                                      dword =
--                                                                        Wordform0{word0 = "it"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = ADVCL, d2 = AS},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 13},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "looking"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 18},
--                                                                      dword =
--                                                                        Wordform0{word0 = "went"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = PUNCT, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 18},
--                                                                      dword =
--                                                                        Wordform0{word0 = "went"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 19},
--                                                                      dword =
--                                                                        Wordform0{word0 = ","}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = MARK, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 24},
--                                                                      dword =
--                                                                        Wordform0{word0 = "lost"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 20},
--                                                                      dword =
--                                                                        Wordform0{word0 = "as"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = MWE, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 20},
--                                                                      dword =
--                                                                        Wordform0{word0 = "as"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 21},
--                                                                      dword =
--                                                                        Wordform0{word0 = "if"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = NSUBJ, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 24},
--                                                                      dword =
--                                                                        Wordform0{word0 = "lost"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 22},
--                                                                      dword =
--                                                                        Wordform0{word0 = "it"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = AUX, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 24},
--                                                                      dword =
--                                                                        Wordform0{word0 = "lost"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 23},
--                                                                      dword =
--                                                                        Wordform0{word0 = "had"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = ADVCL,
--                                                                          d2 = Missing "AS_IF"},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 18},
--                                                                      dword =
--                                                                        Wordform0{word0 = "went"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 24},
--                                                                      dword =
--                                                                        Wordform0{word0 = "lost"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = DOBJ, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 24},
--                                                                      dword =
--                                                                        Wordform0{word0 = "lost"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 25},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "something"}}},
--                                                    Dependence0{dtype =
--                                                                  DepCode{d1 = PUNCT, d2 = DepZero},
--                                                                dgov =
--                                                                  DP0{did = TokenID0{untid0 = 5},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "Rabbit"}},
--                                                                ddep =
--                                                                  DP0{did = TokenID0{untid0 = 26},
--                                                                      dword =
--                                                                        Wordform0{word0 =
--                                                                                    "."}}}]})}],
--               docCorefs =
--                 [Coref0{corefMents =
--                           [Mention0{mentRep = "true", mentSent = SentID0{unSentID0 = 1},
--                                     mentStart = TokenID0{untid0 = 3},
--                                     mentEnd = TokenID0{untid0 = 6},
--                                     mentHead = TokenID0{untid0 = 5},
--                                     mentText = "the White Rabbit"},
--                            Mention0{mentRep = "", mentSent = SentID0{unSentID0 = 1},
--                                     mentStart = TokenID0{untid0 = 1},
--                                     mentEnd = TokenID0{untid0 = 2},
--                                     mentHead = TokenID0{untid0 = 1}, mentText = "It"},
--                            Mention0{mentRep = "", mentSent = SentID0{unSentID0 = 1},
--                                     mentStart = TokenID0{untid0 = 17},
--                                     mentEnd = TokenID0{untid0 = 18},
--                                     mentHead = TokenID0{untid0 = 17}, mentText = "it"},
--                            Mention0{mentRep = "", mentSent = SentID0{unSentID0 = 1},
--                                     mentStart = TokenID0{untid0 = 22},
--                                     mentEnd = TokenID0{untid0 = 23},
--                                     mentHead = TokenID0{untid0 = 22}, mentText = "it"}]}]})]
--

result1E :: ErrOrVal [Maybe (NLPtext, Doc0)]
result1E =
    Right
      [Nothing,
       Just
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
               docCorefs = []}),
       Just
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
                                      tlemma = Lemma0{lemma0 = "br\228uche"}, tbegin = 7, tend = 14,
                                      tpos = NN, tpostt = "", tner = ["O"], tspeaker = []},
                               Token0{tid = TokenID0{untid0 = 3},
                                      tword = Wordform0{word0 = "werden"},
                                      tlemma = Lemma0{lemma0 = "werden"}, tbegin = 15, tend = 21,
                                      tpos = Unk, tpostt = "", tner = ["O"], tspeaker = []},
                               Token0{tid = TokenID0{untid0 = 4},
                                      tword = Wordform0{word0 = "lebendig"},
                                      tlemma = Lemma0{lemma0 = "lebendig"}, tbegin = 22, tend = 30,
                                      tpos = Unk, tpostt = "", tner = ["O"], tspeaker = []}],
                            sdeps = Nothing}],
               docCorefs = []}),
       Just
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
                                      tlemma = Lemma0{lemma0 = "m\246chte"}, tbegin = 28, tend = 34,
                                      tpos = Unk, tpostt = "", tner = ["O"], tspeaker = []},
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
               docCorefs = []}),
       Just
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
                                      tlemma = Lemma0{lemma0 = "paragraph"}, tbegin = 27, tend = 36,
                                      tpos = NN, tpostt = "", tner = ["O"], tspeaker = []},
                               Token0{tid = TokenID0{untid0 = 7}, tword = Wordform0{word0 = "."},
                                      tlemma = Lemma0{lemma0 = "."}, tbegin = 36, tend = 37,
                                      tpos = Unk, tpostt = "", tner = ["O"], tspeaker = []}],
                            sdeps = Nothing}],
               docCorefs = []})]

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
