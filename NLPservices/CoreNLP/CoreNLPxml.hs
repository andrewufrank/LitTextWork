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

import qualified NLP.Types.Tags      as NLP
import qualified NLP.Corpora.Conll  as Conll
import qualified NLP.Corpora.UD  as UD
import              CoreNLP.DEPcodes

import              CoreNLP.NERcodes
import           Uniform.FileIO
import           Text.XML.HXT.Core hiding (when)

import           CoreNLP.Defs0
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
          <start>1</start>
          <end>3</end>
          <head>2</head>
          <text>The uncle</text>
        </mention>
        <mention>
          <sentence>3</sentence>
          <start>2</start>
          <end>3</end>
          <head>2</head>
          <text>he</text>
        </mention>
        <mention>
          <sentence>3</sentence>
          <start>8</start>
          <end>9</end>
          <head>8</head>
          <text>he</text>
        </mention>
      </coreference>
      <coreference>
        <mention representative="true">
          <sentence>3</sentence>
          <start>10</start>
          <end>12</end>
          <head>11</head>
          <text>a book</text>
        </mention>
        <mention>
          <sentence>4</sentence>
          <start>1</start>
          <end>2</end>
          <head>1</head>
          <text>It</text>
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

--getMention  = atTag "mention" >>>
--    proc x -> do
--        r <- getAttrValueT "representative" -<  x
--        s <- text <<< atTag "sentence" -< x
--        st <- text <<< atTag "start" -< x
--        e <- text <<< atTag "end" -< x
--        h <- text <<< atTag "head" -< x
--        t <- text <<< atTag "text" -< x
--        returnA -< Mention0 { mentRep = readRep r
--                        , mentSent =   mkSentID s
--                        , mentStart = mkTokenID st
--                        , mentEnd =  mkTokenID e
--                        , mentHead = mkTokenID h
--                        , mentText = t
--                        }

readRep "true" = True
readRep _ = False

--getCorefOuter = atTag "coreference" >>>   -- outer - clusters
--            -- but only one type of coref assumed
--    proc x -> do
--        c <- listA getCorefClusters -< x
--        returnA -< CorefOuter0 c
--
--getCorefClusters = atTag "coreference" >>>   -- inner should have only one true mention
--    proc x -> do
--        ms <- listA getMention -< x
--        returnA -< CorefCluster0 ms



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
        returnA -< Dependence0 (parseDEPtag . s2t $ t) (s2t t) g d

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
--        c <- getCoref0' -< x
        returnA -< Doc0 s -- c
--      where getCoref0' = getCorefOuter `orElse` (arr $ const zero)

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
                        , tner =  parseNERtagList $ n ++ n2 -- readNoteT "nertag" n
                        , tspeaker =  parseSpeakerTagList  s
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

class (NLP.POStags postag) => ReadDocXML postag where
    -- the operation on the XML doc which depend on the POStag

    readDocString :: postag -> Bool -> Text  -> ErrIO  (Doc0 postag)

instance (NLP.POStags postag) => ReadDocXML postag where

    readDocString ph showXML text = do
        docs   <-callIO $
            runX (readString [withValidate no]  (t2s text)
                                    >>> getDoc0 ph)
--              error "readDocString with showXLM true - intentional stop in corenlpxml.hs line285"
        xmlres <- callIO $ runX . xshow $ readString [withValidate no]  (t2s text)
                                                >>> indentDoc
        when showXML $ do
              putIOwords ["the xml formated"]
              putIOwords  . map s2t $  xmlres
              putIOwords ["the xml formated ---------------------"]
        if length docs > 1
            then error "multiple document tags"
            else   if null docs
                    then throwErrorT ["readDocString", "no document found ", take' 1000 . showT $ xmlres]
                     -- return empty doc if call error - issue with italian
                    else do
                            let doc2 = headNote "no document found" docs
                            return doc2
                -- error in case of 0

parseOne :: (NLP.POStags postag) => postag -> Text -> IO [Doc0 postag]
parseOne ph text = runX (readString [withValidate no]  (t2s text)
                                    >>> getDoc0 ph)
--parseOnee :: (NLP.POStags POStagConll) => postag -> Text -> IO [Doc0 POStagConll]
--parseOnee ph text = runX (readString [withValidate no]  (t2s text)
--                                    >>> getDoc0x)
