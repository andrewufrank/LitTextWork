

-----------------------------------------------------------------------------
--
-- Module      :  Parser . Produce NLP triples
-- Copyright   :  andrew u frank -
--
-- | produce the triples for the NLP part
--
-- version 2 assumes that each paragraph is individually analyzed
--  for german - the lemma are determined for each sentence individually
-- using the tokenization from the coreNLP

-- reduced to a single dependency

-- later - open language changes inside paragraph :
-- snippets are pieces in one paragraph of one languageBreakCode
-- therefo~~~~~re the snippet id is paragraph id + count
--
-- the aggregation of small paragraphs to longer units to snaps will be
-- done later, which will require a snap unit consisting of serveral paragraphs
-- changed, but questions:
    -- why is docsigl = parasigl?
    -- why goes snipets up to 20+ in t9
    -- add sentence part of .. sniped - para - doc
    -- add the redundant info from coref for testing

-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}

module Parser.ProduceNLPtriples
    (module Parser.ProduceNLPtriples
    , module CoreNLP.Defs0
    , module Parser.NLPvocabulary
    , module Lines2para.Lines2para
    ) where

import           Test.Framework
import Uniform.TestHarness (testVar3File)
import Parser.ReadMarkupAB  -- for resultA1 etc.
import CoreNLP.Defs0
import Parser.NLPvocabulary
--import CoreNLP.CoreNLPxml (readDocString)
import Lines2para.Lines2para hiding ((<|>),(</>), (<.>))
import Parser.ProduceDocCallNLP
import Parser.TextDescriptor
import Parser.ProduceLayout (buchURIx)

processDoc0toTriples2 :: TextDescriptor -> LanguageCode -> ParaNum -> (Int, Doc0) -> [Triple] -- TriplesGraph  G -> H
-- ^ convert the doc0 (which is the analysed xml) and produce the triples
processDoc0toTriples2 textstate lang paranr (snipnr, doc0)   =
--        t0 : t1 :
        t2  : sents ++ corefs
                    -- , corefs] corefs not produced
    where
        -- unfertig - snipnr is not yet used.
        -- add a sentence part of para, sentence id made with snip id (paranr - sniptnr)
--        lang = tz3lang ntz
        paraid = paraSigl textstate $ paranr -- . tz3para $ ntz
        snipid = mkSnipSigl paraid snipnr
        buchUri = buchURIx textstate
--        t0 = mkTripleType (unSnipSigl snipid) (mkRDFtype Snip)
--        t1 = mkTriplePartOf (unSnipSigl snipid) (unParaSigl paraid)
        t2 = mkTripleText (unSnipSigl snipid) (mkRDFproperty LanguageTag) (showT lang)
--        t3 = mkTripleType  (unParaSigl paraid) (mkRDFtype Paragraph)
        -- gives two different lit: and nlp:Paragraph types?
        -- t4 gives se
        sents :: [Triple]
        sents =   concat $ map (mkSentenceTriple2 lang buchUri snipid) (docSents doc0)
        corefs = concat $ zipWith (mkCorefTriple2 lang   snipid )
                            (docCorefs doc0) [1 .. ]
-- currently not producing the not yet used corefs

----------------------
mkSentenceTriple2 :: LanguageCode ->  RDFsubj ->  SnipSigl  ->    Sentence0 ->  ( [Triple])
-- ^ produce the   triples for a sentence
mkSentenceTriple2 lang buchuri  snipid sent
       =      t0 : t1 : t2 :  senteceForm : (toktrips ++ depsTrips)
       -- here a strange looping occurs? ?
    where
        sentSigl = mkSentSigl snipid (sid sent)
        t0 = mkTripleType (unSentSigl sentSigl) (mkRDFtype Sentence)
        t1 = mkTripleText   (unSentSigl sentSigl) (mkRDFproperty Parse)
                                (sparse sent)
        t2 = mkTriplePartOf (unSentSigl sentSigl) buchuri
                            -- (unSnipSigl snipid)
        depsTrips =  maybe [] (mkDependenceTypeTriples2 lang sentSigl )
                                ( sdeps sent) :: [Triple]
        toktrips =  concatMap (mkTokenTriple2 lang sentSigl)
                                $ (stoks sent):: [Triple]
        senteceForm = mkTripleLang lang (unSentSigl sentSigl) (mkRDFproperty SentenceForm)
                    (unwords' . map (word0 . tword) . stoks $ sent )

----------------------------------------- --
mkTokenTriple2 :: LanguageCode -> SentSigl-> Token0 -> [Triple]
mkTokenTriple2 lang sentSigl tok =  [t0, t1, t2, t2a, t3, t5] ++ t6 ++ t7
    -- the language code is to pass to the triple maker ! TODO
    where
        tokensigl = mkTokenSigl sentSigl (tid tok)
        t0 = mkTripleType (unTokenSigl tokensigl) (mkRDFtype Token)
        t1 = mkTriplePartOf (unTokenSigl tokensigl)     (unSentSigl sentSigl)
        t2 = mkTripleLang lang (unTokenSigl tokensigl)
                    (mkRDFproperty Lemma) (lemma0 $ tlemma tok)
        t2a = mkTripleLang3 lang (unTokenSigl tokensigl)
                    (mkRDFproperty Lemma3) (lemma0 $ tlemma tok)
        t3 = mkTripleText (unTokenSigl tokensigl)
                    (mkRDFproperty Pos) (show' . tpos $ tok)  -- use the encoding from Conll
        t5 = mkTripleLang lang (unTokenSigl tokensigl) (mkRDFproperty WordForm)
                ( word0 . tword $ tok)
        t6 = map (mkTripleText (unTokenSigl tokensigl)
                (mkRDFproperty Nertag)) (tner tok)
        t7 = map (mkTripleText (unTokenSigl tokensigl)
                    (mkRDFproperty SpeakerTag) . showT )
                    (tspeaker tok)
----------------------------------

mkDependenceTypeTriples2 :: LanguageCode -> SentSigl ->    DependenceType0 -> [Triple]
mkDependenceTypeTriples2 lang sentSigl  d   =  t1 : t2 : ts
-- the selection is already in coreNLPxml, which takes the last of the dep analysis
-- this is too complex and too indirect. link directly to sentence

--    if  dtt d == "enhanced-plus-plus-dependencies"
--    -- use only the best dependency analysis -- could filter out earlier?
--        -- "enhanced-plus-plus-dependencies"
--            then t1 : ts
--            else []
    where
        depTid = mkDepTypeSigl sentSigl  "dependency" -- (dtt d)
        -- only one selected in corenlpxml
        t1 = mkTripleType (unDepTypeSigl depTid) (mkRDFtype DepType)
        t2 = mkTriplePartOf (unDepTypeSigl depTid)     (unSentSigl sentSigl)
        ts = concat $ zipWith (mkDependenceTriple2 lang sentSigl depTid ) ( dtd d)  [1..]
                -- passes sentSigl to construct the tokenid later

mkDependenceTriple2 :: LanguageCode -> SentSigl -> DepTypeSigl -> Dependence0 -> Int -> [Triple]
mkDependenceTriple2 lang sentid depTid dep i  =  t0: t2 : t4 : (t5 ++ t6)
-- dependence construction produces incorrect (white space, " etc in depSigl
    where
        depid = mkDepSigl depTid i
                -- must be numbered - the same code may appear twice (dtype dep)
        dependencyCode = shownice . dtype $ dep
        t0 = mkTripleType (unDepSigl depid) (mkRDFtype Dependence)
        t2 = mkTriplePartOf (unDepSigl depid)     (unDepTypeSigl depTid)
        t4 = mkTripleText (unDepSigl depid) (mkRDFproperty Dependency)(shownice $ dtype dep)
        t5 = mkDependencePart2 lang sentid  depid  GDgov  (dgov dep)
        t6 = mkDependencePart2 lang sentid  depid  GDdep  (ddep dep)

data GOV_DEP = GDgov | GDdep

mkDependencePart2 :: LanguageCode -> SentSigl -> DepSigl -> GOV_DEP -> DependencePart0 -> [Triple]
mkDependencePart2 lang sentid depidp gd depp   = [t8, t9]
    where
       tokenid = mkTokenSigl sentid (did depp)
       prop = case gd of
                GDgov -> (mkRDFproperty Governor)
                GDdep -> (mkRDFproperty Dependent)
       t8 = mkTripleRef (unDepSigl depidp) prop (unTokenSigl tokenid)
       t9 = mkTripleLang lang (unDepSigl depidp) (mkRDFproperty DepWordform) wf
       wf = word0 . dword  $ depp

testOP_F_G :: TextDescriptor -> [Doc0] ->  [Triple]
testOP_F_G textstate docs  = concat
        . map (processDoc0toTriples2 textstate English (ParaNum 99))
        $ (zip [1..] docs)
-- here missing the values for language and paranr
-- fake should be ok for test
--
test_1_F_G :: IO ()
test_1_F_G =  testVar3File result1A "resultF1" "resultG1" testOP_F_G
test_2_F_G =  testVar3File result2A "resultF2" "resultG2" testOP_F_G
test_3_F_G =  testVar3File result3A "resultF3" "resultG3" testOP_F_G
test_4_F_G =  testVar3File result4A "resultF4" "resultG4" testOP_F_G
test_5_F_G =  testVar3File result5A "resultF5" "resultG5" testOP_F_G
test_6_F_G = testVar3File result6A "resultF6" "resultG6" testOP_F_G
--test_7_F_G = testVar3File result6A "resultF7" "resultG7" testOP_F_G
test_8_F_G = testVar3File result8A "resultF8" "resultG8" testOP_F_G
test_9_F_G = testVar3File result9A "resultF9" "resultG9" testOP_F_G
test_10_F_G = testVar3File result10A "resultF10" "resultG10" testOP_F_G
-- 10 seems too big for oporto (without swap)

------------ coreferences ---------------------
-- call         corefs = concat $ map (mkCorefTriple2 lang   snipid ) (docCorefs doc0)

mkCorefTriple2 :: LanguageCode -> SnipSigl ->     Coref0 -> CorefNr ->  [Triple]
-- ^ gives a single set of coreferences  - int is to produce id
mkCorefTriple2 lang snip coref i  = t0 : t1 : concat  coreftrips
    where
        corefid = mkCorefsigl snip i
        coreftrips = zipWith (mkMention2 lang snip  corefid) (corefMents coref) [1 .. ]
        t0 = mkTripleType (unCorefSigl corefid) (mkRDFtype Coreference )
        t1 = mkTriplePartOf (unCorefSigl corefid) (unSnipSigl snip)  -- perhaps not necessary

mkMention2 :: LanguageCode -> SnipSigl ->   CorefSigl ->  Mention0  -> Int -> [Triple]
mkMention2 lang snipid corefsigl m i = [t0, t00, t10, t1, t2, t3, t4, t5]
    where
        mentionid = unMentionSigl $ mkMentionsigl corefsigl i
        sentid = mkSentSigl  snipid (mentSent m)
        t0 = mkTripleType mentionid (mkRDFtype Mention)
        t00 = mkTriplePartOf mentionid (unCorefSigl corefsigl)
        t10 = mkTripleText  mentionid (mkRDFproperty MentionRepresenatative)
                    ( mentRep $ m)
                    -- true for the representative mention - i.e. not a pronoun
        t1 = mkTripleRef mentionid (mkRDFproperty MentionSentence) (unSentSigl sentid)
        t2 = mkTripleRef mentionid (mkRDFproperty MentionSentenceStart)
                                    (unTokenSigl . mkTokenSigl sentid . mentStart $ m)
        t3 = mkTripleRef mentionid (mkRDFproperty MentionSentenceEnd)
                                    (unTokenSigl . mkTokenSigl sentid . mentEnd $ m)
        t4 = mkTripleRef mentionid (mkRDFproperty MentionSentenceHead)
                                    (unTokenSigl . mkTokenSigl  sentid . mentHead $ m)
        t5 = mkTripleLang lang mentionid (mkRDFproperty MentionSentenceText)
                                    ( mentText $ m)
--        TODO

