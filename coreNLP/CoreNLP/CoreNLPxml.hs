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
{-# LANGUAGE Arrows                    #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE UndecidableInstances      #-}

module CoreNLP.CoreNLPxml (readDoc, getDoc0
            , Doc0 (..)
            ) where

import qualified NLP.Corpora.Conll      as Conll
import qualified NLP.Types.Tags         as NLP (Tag (..))
import           Uniform.Error
import           Uniform.FileIO
import           Uniform.Strings

import           Text.XML.HXT.Core

import           CoreNLP.Defs0
import           CoreNLP.DependencyCodes
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

    <coreference>
      <coreference>
        <mention representative="true">
          <sentence>2</sentence>
          <start>24</start>
          <end>27</end>
          <head>24</head>
          <text>thousands of people</text>
        </mention>
        <mention>
          <sentence>2</sentence>
          <start>30</start>
          <end>31</end>
          <head>30</head>
          <text>their</text>
        </mention>
      </coreference>
-}

--atTag :: String ->  XmlTree -> XmlTree
-- ^ find a subtree with a tag
atTag tag = deep (isElem >>> hasName (t2s tag))

--text :: XmlTree->  String
-- ^ get the children from this point
text = getChildren >>> getText >>> arr (s2t)

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
        returnA -< DP0 (TokenID0 $ readNote "governor id" i) (Wordform0   t)


getDependence = atTag "dep" >>>
    proc x -> do
        t <- getAttrValue "type" -<  x
        g <- getGov1 -< x
        d <- getDep1 -< x
        returnA -< Dependence0 (readDepCodes . s2t $ t)
                         g d

--getDependencies = atTag "dependencies" >>>
--    proc x -> do
--        ds <- listA getDependencyType -< x
--        returnA -< ds

getDependencyType = atTag "dependencies" >>>
    proc x -> do
        t <- getAttrValueT "type" -< x
        ds <- listA getDependence -< x
        returnA -< DependenceType0 t ds




--getDoc0 :: _ ->  Doc0
getDoc0 = atTag "document" >>>
    proc x -> do
        s <- getSentences -< x
        c <- getCoref0' -< x
        returnA -< Doc0 s c
      where getCoref0' = (getCoref0)  `orElse` (arr (const []))

getSentences = atTag "sentences" >>>
    proc x -> do
        st <- listA getSentence -< x
        returnA -<   st

getSentence = atTag "sentence" >>>
    proc x -> do
        i <- getAttrValueT "id" -< x
        tks <- getTokens -< x
        ps <- text <<< atTag "parse" -< x
        deps <- listA getDependencyType -< x
        returnA -< Sentence0 {sid = mkSentID  i
                            , sparse = ps
--                            , sdeps = getLastOrNothing deps
                            , sdeps = listToMaybe . reverse  $ deps
                            -- if dependencies are produced, get the last (best?) one
                           , stoks =  tks
                           }

--getLastOrNothing [] = []
--getLastOrNothing as = singleton . headNoteT ["getLastOrNothing in coreNLPxml getDependencyType", showT as] . reverse $ as
--
--singleton a = [a]

getTokens = atTag "tokens" >>>
    proc x -> do
        ts <- listA getToken -< x
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

getToken = atTag "token" >>>
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
                        , tpos = NLP.parseTag  p
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


readDocumentT args lfp = readDocument args (t2fp . filepath2text lpX $ lfp)

readDoc :: LegalPathname   -> ErrIO  Doc0
readDoc fp = do

  docs  :: [Doc0] <-callIO $ do
                d1 :: [Doc0] <-  runX (readDocumentT [withValidate no] fp
                                        >>> getDoc0)   -- getToken1)
                return d1

--  let toks2 = filter ((0 /=). sid) toks
  -- seems to add a lot of empty sentences
  case (length docs) of
    1 -> do
        let d  = headNoteT  ["readDoc:", "no document found, but count 1"
                          , showT docs] (docs :: [Doc0])
        return d
    0 -> do
        putIOwords ["readDoc: count 0", showT docs]
        throwError "readDoc count 0"
    _ -> do
       putIOwords ["readDoc: count ", showT (length docs), showT docs]
       throwError "read doc count more than 1"
