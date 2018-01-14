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

module CoreNLP.CoreNLPxml (
            module CoreNLP.CoreNLPxml
            , module CoreNLP.Defs0
            -- , module CoreNLP.DependencyCodes
            ) where

import           Test.Framework
import Uniform.TestHarness

import qualified NLP.Types.Tags         as NLP (Tag (..))
import qualified CoreNLP.POScodesUD         as UD
import  CoreNLP.POScodesUD         (POStagUD (..))
import  CoreNLP.POScodesConll         ( POStagConll(..))
import CoreNLP.POScodesTinT   -- italian

import           Uniform.Error
import           Uniform.FileIO
import           Uniform.Strings

import           Text.XML.HXT.Core hiding (when)

--import           CoreNLP.POScodes
import           CoreNLP.Defs0
--import NLP.Corpora.Conll (Tag(..))
-- import CoreNLP.ReadDoc0
-- import           CoreNLP.DependencyCodes
import Data.Maybe


{-<root>
  <document>
    <sentences>
      <sentence id="1">
        <tokens>
          <token id="1">
            <word>HANS</word>
            <lemma>HANS</lemma>
            <CharacterOffsetBegin>0</CharacterOffsetBegin>
            <CharacterOffsetEnd>4</CharacterOffsetEnd>
            <POS>NNP</POS>
            <NER>PERSON</NER>
            <Speaker>PER0</Speaker>
          </token>



 <dependencies type="basic-dependencies">
          <dep type="root">
            <governor idx="0">ROOT</governor>
            <dependent idx="7">submitted</dependent>
          </dep>
          <dep type="nsubjpass">
            <governor idx="7">submitted</governor>
            <dependent idx="1">Bills</dependent>
          </dep>

    <coreference>  -- occurs twice
      <coreference>
        <mention representative="true">
          <sentence>1</sentence>
          <start>1</start>
          <end>2</end>
          <head>1</head>
          <text>Alice</text>
        </mention>
        <mention>
          <sentence>1</sentence>
          <start>11</start>
          <end>12</end>
          <head>11</head>
          <text>her</text>
        </mention>
      </coreference>
    </coreference>
-}

--atTag :: String ->  XmlTree -> XmlTree
-- ^ find a subtree with a tag
atTag tag = deep (isElem >>> hasName (t2s tag))

--text :: XmlTree->  String
-- ^ get the children from this point
text = getChildren >>> getText >>> arr s2t

getAttrValueT t = getAttrValue (t2s t) >>> arr s2t

getMention  = atTag "mention" >>>
    proc x -> do
        r <- getAttrValueT "representative" -<  x
        s <- text <<< atTag "sentence" -< x
        st <- text <<< atTag "start" -< x
        e <- text <<< atTag "end" -< x
        h <- text <<< atTag "head" -< x
        t <- text <<< atTag "text" -< x
        returnA -< Mention0 { mentRep = r  -- TODO
                        , mentSent =   mkSentID s
                        , mentStart = mkTokenID st
                        , mentEnd =  mkTokenID e
                        , mentHead = mkTokenID h
                        , mentText = t
                        }

getCoref = atTag "coreference" >>>   -- occurs twice
    proc x -> do
        ms <- listA getMention -< x
        returnA -< Coref0 ms

getCoref0 = atTag "coreference" >>>   -- occurs twice
    proc x -> do
        c <- listA getCoref -< x
        returnA -< c


getGov1 = atTag "governor" >>>
    proc x -> do
        i <- getAttrValue "idx" -< x
        t <- text -< x
        returnA -< DP0 (TokenID0 $ readNote "governor id" i) (Wordform0   t)

getDep1 = atTag "dependent" >>>
    proc x -> do
        i <- getAttrValue "idx" -< x
        t <- text -< x
        returnA -< DP0 (TokenID0 $ readNote "dependent id" i) (Wordform0   t)


getDependence = atTag "dep" >>>
    proc x -> do
        t <- getAttrValue "type" -<  x
        g <- getGov1 -< x
        d <- getDep1 -< x
        returnA -< Dependence0 (readDepCodes . s2t $ t) g d

--getDependencies = atTag "dependencies" >>>
--    proc x -> do
--        ds <- listA getDependencyType -< x
--        returnA -< ds

getDependencyType = atTag "dependencies" >>>
    proc x -> do
        t <- getAttrValueT "type" -< x
        ds <- listA getDependence -< x
        returnA -< DependenceType0 t ds


--getDoc0 :: _ ->  Doc0 postag
getDoc0 ph = atTag "document" >>>
    proc x -> do
        s <- getSentences ph  -< x
        c <- getCoref0' -< x
        returnA -< Doc0 s c
      where getCoref0' = (getCoref0)  `orElse` (arr (const []))

getSentences ph = atTag "sentences" >>>
    proc x -> do
        st <- listA (getSentence ph)-< x
        returnA -<   st

getSentence ph = atTag "sentence" >>>
    proc x -> do
        i <- getAttrValueT "id" -< x
        tks <- getTokens ph -< x
--        ps <- text <<< atTag "parse" -< x
        ps <- getParse' -< x
        deps <- listA getDependencyType -< x
        returnA -< Sentence0 {sid = mkSentID  i
                            , sparse = ps
--                            , sdeps = getLastOrNothing deps
                            , sdeps = listToMaybe . reverse  $ deps
                            -- if dependencies are produced, get the last (best?) one
                           , stoks =  tks
                           }
      where getParse' = (text <<< atTag "parse")  `orElse` (arr (const ""))

--getLastOrNothing [] = []
--getLastOrNothing as = singleton . headNoteT ["getLastOrNothing in coreNLPxml getDependencyType", showT as] . reverse $ as
--
--singleton a = [a]

getTokens ph = atTag "tokens" >>>
    proc x -> do
        ts <- listA (getToken ph) -< x
        returnA -< ts


--    <sentence id="2">
--        <tokens>
--          <token id="1">
--            <word>000</word>
--            <lemma>000</lemma>
--            <CharacterOffsetBegin>2</CharacterOffsetBegin>
--            <CharacterOffsetEnd>5</CharacterOffsetEnd>
--            <POS>CD</POS>
--            <NER>NUMBER</NER>
--            <NormalizedNER>0.0</NormalizedNER>
--          </token>
--          <token id="2">
--            <word>.</word>
--            <lemma>.</lemma>
--            <CharacterOffsetBegin>5</CharacterOffsetBegin>
--            <CharacterOffsetEnd>6</CharacterOffsetEnd>
--            <POS>.</POS>
--            <NER>O</NER>
--          </token>
--        </tokens>

getToken ph = atTag "token" >>>
    proc x -> do
        i <- getAttrValueT "id" -<  x
        w <- text <<< atTag "word" -< x
        l <- text <<< atTag "lemma" -< x
        tb <- text <<< atTag "CharacterOffsetBegin" -< x
        te <- text <<< atTag "CharacterOffsetEnd" -< x
        p <- text <<< atTag "POS" -< x
        n <- listA getNER -< x
--        n <- text <<< atTag "NER" -< x
        n2 <- listA getNormalizedNER -< x
        s <- listA getSpeaker -< x
        returnA -< Token0 { tid = mkTokenID i
                        , tlemma = Lemma0   l
                        , tword = Wordform0 w
                        , tpos = (NLP.parseTag  p) `asTypeOf` ph
                        , tposOrig = p  -- the text received
                        , tpostt = zero
                        , tner = n ++ n2 -- readNoteT "nertag" n
                        , tspeaker = map readSpeakerTag  s
                        , tbegin = readNoteT "tbegin" tb
                        , tend = readNoteT "tend" te
                        }

getNER = atTag "NER" >>>
    proc x -> do
        nx <- text -< x
        returnA -< nx

getNormalizedNER = atTag "NormalizedNER" >>>
    proc x -> do
        nx <- text -< x
        returnA -< nx

getSpeaker = atTag "Speaker" >>>
    proc x -> do
        nx <- text -< x
        returnA -< nx

class (NLP.Tag postag) => ReadDocXML postag where
    -- the operation on the XML doc which depend on the POStag

    readDocString :: postag -> Bool -> Text  -> ErrIO  (Doc0 postag)

instance (NLP.Tag postag) => ReadDocXML postag where

    readDocString ph showXML text = do
        docs   <-callIO $
            runX (readString [withValidate no]  (t2s text)
                                    >>> getDoc0 ph)
        when showXML $ do
              putIOwords ["the xml formated"]
--              error "readDocString with showXLM true - intentional stop in corenlpxml.hs line285"
              res <- callIO $ runX . xshow $ readString [withValidate no]  (t2s text)
                                                >>> indentDoc
              putIOwords  $ map s2t res
              putIOwords ["the xml formated ---------------------"]
        if length docs > 1
            then error "multiple document tags"
            else   if null docs
                    then throwErrorT ["readDocString", "no document found in readDocString"]
                     -- return empty doc if call error - issue with italian
                    else do
                            let doc2 = (headNote "no document found" docs)
                            return doc2
                -- error in case of 0

parseOne :: (NLP.Tag postag) => postag -> Text -> IO [Doc0 postag]
parseOne ph text = runX (readString [withValidate no]  (t2s text)
                                    >>> getDoc0 ph)
--parseOnee :: (NLP.Tag POStagConll) => postag -> Text -> IO [Doc0 POStagConll]
--parseOnee ph text = runX (readString [withValidate no]  (t2s text)
--                                    >>> getDoc0x)
test_xml_0_Engl :: IO ()
test_xml_0_Engl = do
        r <-parseOne (undef "xx32" :: POStagConll) doc001
        putIOwords ["test_xml_0_Engl", showT r]
--        assertEqual ([Doc0{docSents = [], docCorefs = []}]::[Doc0 POStagConll]) r
        assertEqual resEng r

test_xml_0_DU :: IO ()
test_xml_0_DU = do
        r <-parseOne (undef "xx33" :: POStagUD) doc001 -- no parse RD is not a UD POS tag. what is it?
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

resEng :: [Doc0 POStagConll]
resEng = [Doc0{docSents =
        [Sentence0{sid = SentID0{unSentID0 = 1}, sparse = "",
                   stoks =
                     [Token0{tid = TokenID0{untid0 = 1},
                             tword = Wordform0{word0 = "Lo"}, tlemma = Lemma0{lemma0 = "il"},
                             tbegin = 0, tend = 2, tpos = Unk, tpostt = "", tner = ["O"],
                             tposOrig = "RD" ,
                             tspeaker = []}],
                   sdeps = Nothing}],
      docCorefs = []}]

resUD :: [Doc0 POStagUD]
resUD = [Doc0{docSents =
                [Sentence0{sid = SentID0{unSentID0 = 1}, sparse = "",
                  stoks =
                     [Token0{tid = TokenID0{untid0 = 1},
                             tword = Wordform0{word0 = "Lo"}, tlemma = Lemma0{lemma0 = "il"},
                             tbegin = 0, tend = 2, tpos = X, tpostt = "", tner = ["O"],
                             tposOrig = "RD",
                             tspeaker = []}],
                   sdeps = Nothing}],
      docCorefs = []}]

test_parsePosConll = assertEqual (Unk::POStagConll)  (NLP.parseTag  "xx")
test_parsePosUD = assertEqual (X::POStagUD)  (NLP.parseTag  "xx")

----getDoc0 :: _ ->  Doc0 postag
--getDoc0x   = atTag "document" >>>
--    proc x -> do
----        s <- getSentences ph  -< x
----        c <- getCoref0' -< x
--        returnA -< Doc0 [] []
----      where getCoref0' = (getCoref0)  `orElse` (arr (const []))

-- ende
