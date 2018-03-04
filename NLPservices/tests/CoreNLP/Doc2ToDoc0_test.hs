-----------------------------------------------------------------------------
--
-- Module      :   a test for HTF framework
-- insert {-@ HTF_TESTS @-} for each import
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}
module Lib.Doc2ToDoc0_test  -- (openMain, htf_thisModuelsTests)
     where


import           Test.Framework
import           Uniform.Strings
import Uniform.FileIO
--import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as B
import Data.Aeson (eitherDecode)

import Lib.ParseJsonCoreNLP
import Lib.Doc2ToDoc0
import qualified NLP.Corpora.Conll  as Conll
--import Text.Show.Pretty (valToStr)
--import Text.PrettyPrint.GenericPretty

test_nlpjson2  = do
    res0 <- runErr $ do
        let fn = makeRelFile "short1.json"
        putIOwords ["nlp json decode:", showT fn]
        f <- readFile2  fn
--        putIOwords ["json input:",showStartJson f]
        let r = eitherDecode  f  :: Either String Doc2
        let r2 = fmap (doc2to1 Conll.undefConll) r :: Either String (Doc1 Conll.POStag)
        let r3 = fmap id r2
--        putIOwords ["converted", showT r2]
        runErrorFromEither r3
--        return r
    assertEqual restest_nlpjson2  (show res0)

restest_nlpjson2 =  "Right (Doc1 {docSents = [Sentence1 {s1id = SentID0 {unSentID0 = 0}, s1parse = \"(ROOT\\n  (S\\n    (SBAR\\n      (WHADVP (WRB When))\\n      (S\\n        (NP (DT the) (NN uncle))\\n        (VP (VBD came)\\n          (PP (IN into)\\n            (NP (DT the) (NN room))))))\\n    (, ,)\\n    (NP (PRP he))\\n    (VP (VBD carried)\\n      (NP (DT a) (NN book)))\\n    (. .)))\", s1toks = [Token0 {tid = TokenID0 {untid0 = 1}, tword = Wordform0 {word0 = \"When\"}, tlemma = Lemma0 {lemma0 = \"when\"}, tbegin = 0, tend = 4, tpos = WRB, tposOrig = \"WRB\", tpostt = \"\", tner = [O], tspeaker = [SpeakerNumber \"0\"]},Token0 {tid = TokenID0 {untid0 = 2}, tword = Wordform0 {word0 = \"the\"}, tlemma = Lemma0 {lemma0 = \"the\"}, tbegin = 5, tend = 8, tpos = DT, tposOrig = \"DT\", tpostt = \"\", tner = [O], tspeaker = [SpeakerNumber \"0\"]},Token0 {tid = TokenID0 {untid0 = 3}, tword = Wordform0 {word0 = \"uncle\"}, tlemma = Lemma0 {lemma0 = \"uncle\"}, tbegin = 9, tend = 14, tpos = NN, tposOrig = \"NN\", tpostt = \"\", tner = [O], tspeaker = [SpeakerNumber \"0\"]},Token0 {tid = TokenID0 {untid0 = 4}, tword = Wordform0 {word0 = \"came\"}, tlemma = Lemma0 {lemma0 = \"come\"}, tbegin = 15, tend = 19, tpos = VBD, tposOrig = \"VBD\", tpostt = \"\", tner = [O], tspeaker = [SpeakerNumber \"0\"]},Token0 {tid = TokenID0 {untid0 = 5}, tword = Wordform0 {word0 = \"into\"}, tlemma = Lemma0 {lemma0 = \"into\"}, tbegin = 20, tend = 24, tpos = IN, tposOrig = \"IN\", tpostt = \"\", tner = [O], tspeaker = [SpeakerNumber \"0\"]},Token0 {tid = TokenID0 {untid0 = 6}, tword = Wordform0 {word0 = \"the\"}, tlemma = Lemma0 {lemma0 = \"the\"}, tbegin = 25, tend = 28, tpos = DT, tposOrig = \"DT\", tpostt = \"\", tner = [O], tspeaker = [SpeakerNumber \"0\"]},Token0 {tid = TokenID0 {untid0 = 7}, tword = Wordform0 {word0 = \"room\"}, tlemma = Lemma0 {lemma0 = \"room\"}, tbegin = 29, tend = 33, tpos = NN, tposOrig = \"NN\", tpostt = \"\", tner = [O], tspeaker = [SpeakerNumber \"0\"]},Token0 {tid = TokenID0 {untid0 = 8}, tword = Wordform0 {word0 = \",\"}, tlemma = Lemma0 {lemma0 = \",\"}, tbegin = 33, tend = 34, tpos = Comma, tposOrig = \",\", tpostt = \"\", tner = [O], tspeaker = [SpeakerNumber \"0\"]},Token0 {tid = TokenID0 {untid0 = 9}, tword = Wordform0 {word0 = \"he\"}, tlemma = Lemma0 {lemma0 = \"he\"}, tbegin = 35, tend = 37, tpos = PRP, tposOrig = \"PRP\", tpostt = \"\", tner = [O], tspeaker = [SpeakerNumber \"0\"]},Token0 {tid = TokenID0 {untid0 = 10}, tword = Wordform0 {word0 = \"carried\"}, tlemma = Lemma0 {lemma0 = \"carry\"}, tbegin = 38, tend = 45, tpos = VBD, tposOrig = \"VBD\", tpostt = \"\", tner = [O], tspeaker = [SpeakerNumber \"0\"]},Token0 {tid = TokenID0 {untid0 = 11}, tword = Wordform0 {word0 = \"a\"}, tlemma = Lemma0 {lemma0 = \"a\"}, tbegin = 46, tend = 47, tpos = DT, tposOrig = \"DT\", tpostt = \"\", tner = [O], tspeaker = [SpeakerNumber \"0\"]},Token0 {tid = TokenID0 {untid0 = 12}, tword = Wordform0 {word0 = \"book\"}, tlemma = Lemma0 {lemma0 = \"book\"}, tbegin = 48, tend = 52, tpos = NN, tposOrig = \"NN\", tpostt = \"\", tner = [O], tspeaker = [SpeakerNumber \"0\"]},Token0 {tid = TokenID0 {untid0 = 13}, tword = Wordform0 {word0 = \".\"}, tlemma = Lemma0 {lemma0 = \".\"}, tbegin = 52, tend = 53, tpos = Term, tposOrig = \".\", tpostt = \"\", tner = [O], tspeaker = [SpeakerNumber \"0\"]}], s1deps = Just [Dependence1 {d1type = DepCode {d1 = ROOT, d2 = Dep2Zero}, d1orig = \"ROOT\", d1govid = TokenID0 {untid0 = 0}, d1depid = TokenID0 {untid0 = 10}, d1govGloss = \"ROOT\", d1depGloss = \"carried\"},Dependence1 {d1type = DepCode {d1 = ADVMOD, d2 = Dep2Zero}, d1orig = \"advmod\", d1govid = TokenID0 {untid0 = 4}, d1depid = TokenID0 {untid0 = 1}, d1govGloss = \"came\", d1depGloss = \"When\"},Dependence1 {d1type = DepCode {d1 = DET, d2 = Dep2Zero}, d1orig = \"det\", d1govid = TokenID0 {untid0 = 3}, d1depid = TokenID0 {untid0 = 2}, d1govGloss = \"uncle\", d1depGloss = \"the\"},Dependence1 {d1type = DepCode {d1 = NSUBJ, d2 = Dep2Zero}, d1orig = \"nsubj\", d1govid = TokenID0 {untid0 = 4}, d1depid = TokenID0 {untid0 = 3}, d1govGloss = \"came\", d1depGloss = \"uncle\"},Dependence1 {d1type = DepCode {d1 = ADVCL, d2 = Dep2Zero}, d1orig = \"advcl\", d1govid = TokenID0 {untid0 = 10}, d1depid = TokenID0 {untid0 = 4}, d1govGloss = \"carried\", d1depGloss = \"came\"},Dependence1 {d1type = DepCode {d1 = CASE, d2 = Dep2Zero}, d1orig = \"case\", d1govid = TokenID0 {untid0 = 7}, d1depid = TokenID0 {untid0 = 5}, d1govGloss = \"room\", d1depGloss = \"into\"},Dependence1 {d1type = DepCode {d1 = DET, d2 = Dep2Zero}, d1orig = \"det\", d1govid = TokenID0 {untid0 = 7}, d1depid = TokenID0 {untid0 = 6}, d1govGloss = \"room\", d1depGloss = \"the\"},Dependence1 {d1type = DepCode {d1 = NMOD, d2 = INTO}, d1orig = \"nmod:into\", d1govid = TokenID0 {untid0 = 4}, d1depid = TokenID0 {untid0 = 7}, d1govGloss = \"came\", d1depGloss = \"room\"},Dependence1 {d1type = DepCode {d1 = PUNCT, d2 = Dep2Zero}, d1orig = \"punct\", d1govid = TokenID0 {untid0 = 10}, d1depid = TokenID0 {untid0 = 8}, d1govGloss = \"carried\", d1depGloss = \",\"},Dependence1 {d1type = DepCode {d1 = NSUBJ, d2 = Dep2Zero}, d1orig = \"nsubj\", d1govid = TokenID0 {untid0 = 10}, d1depid = TokenID0 {untid0 = 9}, d1govGloss = \"carried\", d1depGloss = \"he\"},Dependence1 {d1type = DepCode {d1 = DET, d2 = Dep2Zero}, d1orig = \"det\", d1govid = TokenID0 {untid0 = 12}, d1depid = TokenID0 {untid0 = 11}, d1govGloss = \"book\", d1depGloss = \"a\"},Dependence1 {d1type = DepCode {d1 = DOBJ, d2 = Dep2Zero}, d1orig = \"dobj\", d1govid = TokenID0 {untid0 = 10}, d1depid = TokenID0 {untid0 = 12}, d1govGloss = \"carried\", d1depGloss = \"book\"},Dependence1 {d1type = DepCode {d1 = PUNCT, d2 = Dep2Zero}, d1orig = \"punct\", d1govid = TokenID0 {untid0 = 10}, d1depid = TokenID0 {untid0 = 13}, d1govGloss = \"carried\", d1depGloss = \".\"}]},Sentence1 {s1id = SentID0 {unSentID0 = 1}, s1parse = \"(ROOT\\n  (S\\n    (NP (PRP It))\\n    (VP (VBD was)\\n      (ADJP (JJ red)))\\n    (. .)))\", s1toks = [Token0 {tid = TokenID0 {untid0 = 1}, tword = Wordform0 {word0 = \"It\"}, tlemma = Lemma0 {lemma0 = \"it\"}, tbegin = 54, tend = 56, tpos = PRP, tposOrig = \"PRP\", tpostt = \"\", tner = [O], tspeaker = [SpeakerNumber \"0\"]},Token0 {tid = TokenID0 {untid0 = 2}, tword = Wordform0 {word0 = \"was\"}, tlemma = Lemma0 {lemma0 = \"be\"}, tbegin = 57, tend = 60, tpos = VBD, tposOrig = \"VBD\", tpostt = \"\", tner = [O], tspeaker = [SpeakerNumber \"0\"]},Token0 {tid = TokenID0 {untid0 = 3}, tword = Wordform0 {word0 = \"red\"}, tlemma = Lemma0 {lemma0 = \"red\"}, tbegin = 61, tend = 64, tpos = JJ, tposOrig = \"JJ\", tpostt = \"\", tner = [O], tspeaker = [SpeakerNumber \"0\"]},Token0 {tid = TokenID0 {untid0 = 4}, tword = Wordform0 {word0 = \".\"}, tlemma = Lemma0 {lemma0 = \".\"}, tbegin = 64, tend = 65, tpos = Term, tposOrig = \".\", tpostt = \"\", tner = [O], tspeaker = [SpeakerNumber \"0\"]}], s1deps = Just [Dependence1 {d1type = DepCode {d1 = ROOT, d2 = Dep2Zero}, d1orig = \"ROOT\", d1govid = TokenID0 {untid0 = 0}, d1depid = TokenID0 {untid0 = 3}, d1govGloss = \"ROOT\", d1depGloss = \"red\"},Dependence1 {d1type = DepCode {d1 = NSUBJ, d2 = Dep2Zero}, d1orig = \"nsubj\", d1govid = TokenID0 {untid0 = 3}, d1depid = TokenID0 {untid0 = 1}, d1govGloss = \"red\", d1depGloss = \"It\"},Dependence1 {d1type = DepCode {d1 = COP, d2 = Dep2Zero}, d1orig = \"cop\", d1govid = TokenID0 {untid0 = 3}, d1depid = TokenID0 {untid0 = 2}, d1govGloss = \"red\", d1depGloss = \"was\"},Dependence1 {d1type = DepCode {d1 = PUNCT, d2 = Dep2Zero}, d1orig = \"punct\", d1govid = TokenID0 {untid0 = 3}, d1depid = TokenID0 {untid0 = 4}, d1govGloss = \"red\", d1depGloss = \".\"}]}], docCorefs = Coreferences0 {coChains = [MentionChain0 [Mention0 {mentRep = True, mentSent = SentID0 {unSentID0 = 1}, mentStart = TokenID0 {untid0 = 2}, mentEnd = TokenID0 {untid0 = 4}, mentHead = TokenID0 {untid0 = 3}, mentText = \"the uncle\"},Mention0 {mentRep = False, mentSent = SentID0 {unSentID0 = 1}, mentStart = TokenID0 {untid0 = 9}, mentEnd = TokenID0 {untid0 = 10}, mentHead = TokenID0 {untid0 = 9}, mentText = \"he\"}],MentionChain0 [Mention0 {mentRep = True, mentSent = SentID0 {unSentID0 = 1}, mentStart = TokenID0 {untid0 = 11}, mentEnd = TokenID0 {untid0 = 13}, mentHead = TokenID0 {untid0 = 12}, mentText = \"a book\"}],MentionChain0 [Mention0 {mentRep = True, mentSent = SentID0 {unSentID0 = 1}, mentStart = TokenID0 {untid0 = 6}, mentEnd = TokenID0 {untid0 = 8}, mentHead = TokenID0 {untid0 = 7}, mentText = \"the room\"},Mention0 {mentRep = False, mentSent = SentID0 {unSentID0 = 2}, mentStart = TokenID0 {untid0 = 1}, mentEnd = TokenID0 {untid0 = 2}, mentHead = TokenID0 {untid0 = 1}, mentText = \"It\"}]]}})"


showStartJson = s2t . take 100 . B.toString

runErrorFromEither :: (Show s, CharChains s) => Either s a -> ErrIO a
runErrorFromEither (Left s) = throwErrorT ["runErrorFromEither", (toText s)]
                -- (toString s) -- (toText s)
runErrorFromEither (Right a) = return a

---- show produces the "xx"
--test_1 = do
--    res0 <- runErr $ do
--        let fn = makeRelFile "short1.json"
--        putIOwords ["nlp json decode:", showT fn]
--        f <- readFile2  fn
--        putIOwords ["json input:", take' 100 . showT $ f]
--        let r = decodeDoc1 f  :: Either String [Doc1]
--        putIOwords ["decoded:", showT r]
--        return r
--    assertEqual res (show res0)
--
--res =  ""


