-- ---------------------------------------------------------------------------
--
-- Module      :  CoreNLPxml
--
-- | to read the stanford core nlp output in xml format
-- produced by :
-- java -cp stanford-corenlp-3.5.2.jar:stanford-corenlp-3.5.2-models.jar
-- :xom.jar:joda-time.jar:jollyday.jar:ejml-3.5.2.jar
--- Xmx2g edu.stanford.nlp.pipeline.StanfordCoreNLP
-- [ -props af.properties ]
--  -file pg2591_grimm_gutenberg.txt
-- with:
---- annotators = tokenize, ssplit, pos, lemma, ner, parse, dcoref
-- offen:
-- title separately as a sentence?
-- preparing guttenberg texts:
-- delete non-text matter
-- add . or (.title.) after each title (in grimm they are all caps
-- break in files of 1000 +- lines (take 5-10 min to process)
--
-- gives the structure in Defs0 (not using Defs1)
-- limit: the descriptions are local (just reading the xml)
-- produces sent files, which are just the Defs0
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE Arrows                    #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
-- {-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables
    , UndecidableInstances
         , TypeSynonymInstances
     #-}
-- {-# LANGUAGE UndecidableInstances      #-}

module CoreNLP.CoreNLPxml_test  where

import           Test.Framework
import Uniform.TestHarness

import qualified NLP.Types.Tags      as NLP
import qualified NLP.Corpora.Conll  as Conll
import qualified NLP.Corpora.UD  as UD
import              CoreNLP.DEPcodes

import              CoreNLP.NERcodes
import              CoreNLP.CoreNLPxml
--
--
--import           Uniform.Error
--import           Uniform.FileIO
--import           Uniform.Strings
--
--import           Text.XML.HXT.Core hiding (when)
--import           CoreNLP.Defs0
import Data.Maybe


test_xml_0_Engl :: IO ()
test_xml_0_Engl = do
        r <-parseOne (undef "xx32" :: Conll.POStag ) doc001
        putIOwords ["test_xml_0_Engl", showT r]
--        assertEqual ([Doc0{docSents = [], docCorefs = []}]::[Doc0 POStagConll]) r
        assertEqual resEng r

test_xml_0_DU :: IO ()
test_xml_0_DU = do
        r <-parseOne (undef "xx33" :: UD.POStag) doc001 -- no parse RD is not a UD POS tag. what is it?
        putIOwords ["test_xml_0_DU", showT r]
        assertEqual resUD r

doc001 ::Text
-- code is RD but recognized as Unk
--doc001 = "</ source=\"<?xml version=\"1.0\" encoding=\"UTF-8\"?><root>   <document>    <docDate>2018-01-09</docDate>    <sentences>      <sentence id=\"1\">        <tokens>          <token id=\"1\">            <word>Lo</word>            <lemma>il</lemma>            <CharacterOffsetBegin>0</CharacterOffsetBegin>            <CharacterOffsetEnd>2</CharacterOffsetEnd>            <POS>RD</POS>            <NER>O</NER>          </token>        </tokens>    </sentence>    </sentences>  </document></root><//>"
doc001 = "<root>   <document>    <docDate>2018-01-09</docDate>    <sentences>      <sentence id=\"1\">        <tokens>          <token id=\"1\">            <word>Lo</word>            <lemma>il</lemma>            <CharacterOffsetBegin>0</CharacterOffsetBegin>            <CharacterOffsetEnd>2</CharacterOffsetEnd>            <POS>RD</POS>            <NER>O</NER>          </token>        </tokens>    </sentence>    </sentences>  </document></root>"

doc001ud ::Text
-- code is X
--doc001 = "</ source=\"<?xml version=\"1.0\" encoding=\"UTF-8\"?><root>   <document>    <docDate>2018-01-09</docDate>    <sentences>      <sentence id=\"1\">        <tokens>          <token id=\"1\">            <word>Lo</word>            <lemma>il</lemma>            <CharacterOffsetBegin>0</CharacterOffsetBegin>            <CharacterOffsetEnd>2</CharacterOffsetEnd>            <POS>RD</POS>            <NER>O</NER>          </token>        </tokens>    </sentence>    </sentences>  </document></root><//>"
doc001ud = "<root>   <document>    <docDate>2018-01-09</docDate>    <sentences>      <sentence id=\"1\">        <tokens>          <token id=\"1\">            <word>Lo</word>            <lemma>il</lemma>            <CharacterOffsetBegin>0</CharacterOffsetBegin>            <CharacterOffsetEnd>2</CharacterOffsetEnd>            <POS>X</POS>            <NER>O</NER>          </token>        </tokens>    </sentence>    </sentences>  </document></root>"

resEng :: [Doc0 Conll.POStag]
resEng = [Doc0{docSents =
        [Sentence0{sid = SentID0{unSentID0 = 1}, sparse = "",
                   stoks =
                     [Token0{tid = TokenID0{untid0 = 1},
                             tword = Wordform0{word0 = "Lo"}, tlemma = Lemma0{lemma0 = "il"},
                             tbegin = 0, tend = 2, tpos = Conll.Unk, tpostt = "", tner = [O],
                             tposOrig = "RD" ,
                             tspeaker = []}],
                   sdeps = Nothing}],
      docCorefs = []}]

resUD :: [Doc0 UD.POStag]
resUD = [Doc0{docSents =
                [Sentence0{sid = SentID0{unSentID0 = 1}, sparse = "",
                  stoks =
                     [Token0{tid = TokenID0{untid0 = 1},
                             tword = Wordform0{word0 = "Lo"}, tlemma = Lemma0{lemma0 = "il"},
                             tbegin = 0, tend = 2, tpos = UD.X, tpostt = "", tner = [O],
                             tposOrig = "RD",
                             tspeaker = []}],
                   sdeps = Nothing}],
      docCorefs = []}]

test_parsePosConll = assertEqual (Conll.Unk::Conll.POStag)  (NLP.parseTag  "xx")
test_parsePosUD = assertEqual (UD.X::UD.POStag)  (NLP.parseTag  "xx")

----getDoc0 :: _ ->  Doc0 postag
--getDoc0x   = atTag "document" >>>
--    proc x -> do
----        s <- getSentences ph  -< x
----        c <- getCoref0' -< x
--        returnA -< Doc0 [] []
----      where getCoref0' = (getCoref0)  `orElse` (arr (const []))

-- ende
