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

-- later - open language changes inside paragraph :
-- snippets are pieces in one paragraph of one languageBreakCode
-- therefo~~~~~re the snippet id is paragraph id + count
--
-- the aggregation of small paragraphs to longer units to snaps will be
-- done later, which will require a snap unit consisting of serveral paragraphs

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


processDoc0toTriples2 :: TextState2 -> (NLPtext,  Doc0) -> [Triple] -- TriplesGraph  G -> H
-- ^ convert the doc0 (which is the analysed xml) and produce the triples
processDoc0toTriples2 textstate (ntz, doc0)  =       t2  :  sents
                    -- , corefs] corefs not produced
    where
        lang = tz3lang ntz
        paraid = paraSigl textstate . tz3para $ ntz
        snipid = paraid
        t2 = mkTripleText (unParaSigl snipid) (mkRDFproperty LanguageTag) (showT lang)
        sents :: [Triple]
        sents =   concat $ map (mkSentenceTriple2 lang  snipid) (docSents doc0)
 --        corefs = concat $ map (mkCorefTriple2 lang   snipid ) (docCorefs doc0)
-- currently not producing the not yet used corefs

----------------------
mkSentenceTriple2 :: LanguageCode ->   DocSigl  ->    Sentence0 ->  ( [Triple])
-- ^ produce the   triples for a sentence
mkSentenceTriple2 lang   snipid sent
       =      t0 : t1 : t2 :  senteceForm : (toktrips ++ depsTrips)
       -- here a strange looping occurs? ?
    where
        sentSigl = mkSentSigl snipid (sid sent)
        t0 = mkTripleType (unSentSigl sentSigl) (mkRDFtype Sentence)
        t1 = mkTripleText   (unSentSigl sentSigl) (mkRDFproperty Parse)
                                (sparse sent)
        t2 = mkTriplePartOf (unSentSigl sentSigl) (unDocSigl snipid)
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
mkDependenceTriple2 lang sentid depTid dep i  =   t2 : t4 : (t5 ++ t6)
-- dependence construction produces incorrect (white space, " etc in depSigl
    where
        depid = mkDepSigl depTid i
                -- must be numbered - the same code may appear twice (dtype dep)
        dependencyCode = shownice . dtype $ dep
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

testOP_F_G :: TextState2 -> [ (NLPtext,Doc0)] ->  [Triple]
testOP_F_G textstate  = concat . map (processDoc0toTriples2 textstate)
--
test_1_F_G :: IO ()
test_1_F_G =  testVar3File result1A "resultF1" "resultG1" testOP_F_G
test_2_F_G =  testVar3File result2A "resultF2" "resultG2" testOP_F_G
test_3_F_G =  testVar3File result3A "resultF3" "resultG3" testOP_F_G
test_4_F_G =  testVar3File result4A "resultF4" "resultG4" testOP_F_G
test_5_F_G =  testVar3File result5A "resultF5" "resultG5" testOP_F_G
test_6_F_G = testVar3File result6A "resultF6" "resultG6" testOP_F_G
test_7_F_G = testVar3File result6A "resultF7" "resultG7" testOP_F_G
test_8_F_G = testVar3File result8A "resultF8" "resultG8" testOP_F_G
