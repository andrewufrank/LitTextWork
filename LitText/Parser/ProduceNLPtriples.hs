-----------------------------------------------------------------------------
--
-- Module      :  Parser . Produce NLP triples
-- Copyright   :  andrew u frank -
--
-- | the processing with NLP processors are in ProduceNLP
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
    ) where

import           Test.Framework

import Parser.Foundation  hiding ((<|>),(</>), (<.>)) -- for TZ
import Parser.ProduceDocCallNLP
import Uniform.Error   -- For ErrOrVal
import           Data.RDF   -- should all be imported from extension
import          Data.RDF.FileTypes
import Lines2para.Lines2para hiding ((<|>),(</>), (<.>))
import           Parser.NLPvocabulary hiding ((<|>),(</>), (<.>))
import           Uniform.Strings              hiding ((<|>))
import Uniform.FileIO hiding ((</>), (<.>))
import Parser.CompleteSentence -- (completeSentence)
-- for tests
import Parser.ReadMarkupAB -- (result1A)
import Data.Either


processDoc0toTriples2 :: TextState2 -> TZ2 ->  Doc0 -> [Triple] -- TriplesGraph  G -> H
-- ^ convert the doc0 (which is the analysed xml) and produce the triples
processDoc0toTriples2 textstate tz  doc0  =       t2  :  concat [sents]
                    -- , corefs] corefs not produced
    where
        lang = tz2lang tz
        paraid = paraSigl textstate tz
        snipid = paraid
        t2 = mkTripleText (unParaSigl snipid) (mkRDFproperty LanguageTag) (showT lang)
        sents :: [Triple]
        sents = concat $ map (mkSentenceTriple2 lang  snipid) (docSents doc0)
 --        corefs = concat $ map (mkCorefTriple2 lang   snipid ) (docCorefs doc0)
-- currently not producing the not yet used corefs


----------------------
mkSentenceTriple2 :: LanguageCode ->   DocSigl  ->    Sentence0 ->  ( [Triple])
-- ^ produce the   triples for a sentence
-- filters out empty (. only) sentences
-- and recognized the count sentences
mkSentenceTriple2 lang   snipid sent
--       | isEmptySentence =(paragraphcount0,[])
--       | isCountSentence =   (paragraphcount, []) -- [paragraphcountTriple])
       -- only for count sentence change paragraph count
--       | otherwise
       =  t0 : t1 : t2 : senteceForm : (toktrips ++ depsTrips)
    where

        sentSigl = mkSentSigl snipid (sid sent)
        t0 = mkTripleType (unSentSigl sentSigl) (mkRDFtype Sentence)
        t1 = mkTripleText   (unSentSigl sentSigl) (mkRDFproperty Parse)
                                (sparse sent)
        t2 = mkTriplePartOf (unSentSigl sentSigl) (unDocSigl snipid)
        depsTrips = maybe [] (mkDependenceTypeTriples2 lang sentSigl )
                                ( sdeps sent) :: [Triple]
        toktrips = concat . map (mkTokenTriple2 lang sentSigl)
                                $ (stoks sent):: [Triple]
--        toktrips = (mkTokenTriple2 lang sentSigl) $ (concat . stoks $ sent):: [Triple]
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
        -- t4 = mkTripleLang lang tokenid lemmapPoSProp $ mkLemmaPoS (tlemma tok) (tpos tok)
--            ((lemma0 .t1lemma $ tok) <:> (show' . t1pos $tok))
        t5 = mkTripleLang lang (unTokenSigl tokensigl) (mkRDFproperty WordForm)
                ( word0 . tword $ tok)
        t6 = map (mkTripleText (unTokenSigl tokensigl)
                (mkRDFproperty Nertag)) (tner tok)
        t7 = map (mkTripleText (unTokenSigl tokensigl)
                    (mkRDFproperty SpeakerTag) . showT )
                    (tspeaker tok)


mkLemmaPoS :: Lemma0 -> Pos -> Text
-- to avoid including punctuation characters in the lemmaPoS
-- TODO check
mkLemmaPoS l p = if isPOSpunctuation p then show' p
                    else  (lemma0 l) <:> (show' p)

----------------------------------
--
---- dependencies and coref code (from previous repo8 revision 6045)
--
--mkDependenceTypeTriples :: LanguageCode -> SentSigl ->    DependenceType0 -> [Triple]
--mkDependenceTypeTriples lang sentid  d   =
--    if  dtt d == "enhanced-plus-plus-dependencies"
--    -- use only the best dependency analysis -- could filter out earlier?
--            then t1 : ts
--            else []
--    where
--        depTid = mkDepTypeSigl sentid  (dtt d)
--        t1 = mkTripleType (unDepTypeSigl depTid) (mkRDFtype DepType)
--        ts = concat $ map (mkDependenceTriple lang sentid depTid ) ( dtd d) :: [Triple]
--                -- passes sentid to construct the tokenid later
--
----        toktrips =
------        t3 = mkTriplePartOf depid sid
----        t2 = mkTriplePartOf depid (mkSentID . d1sent $ d)
----        depsType = s2t . dt1t $ dType
----        deps2 = zip [1..] (depid dType)
----        trips = concat . map (mkDependence1Triple sid depsType) $ deps2
--
----
--mkDependenceTriple :: LanguageCode -> SentSigl -> DepTypeSigl -> Dependence0 -> [Triple]
--mkDependenceTriple lang sentid depTid dep   = []  -- t4 : (t5 ++ t6)
---- dependence construction produces incorrect (white space, " etc in depSigl
--    where
--        depid = mkDepSigl depTid (dtype dep)
----        t2oURI <#> concat ["Dependence-", mkDepID sid i]
--
----        t0 = mkTripleText depid  depsType
--
--        t4 = mkTripleText (unDepSigl depid) (mkRDFproperty Dependency)  (showT $ dtype dep)
--        t5 = mkDependencePart lang sentid  depid  GDgov  (dgov dep)
--        t6 = mkDependencePart lang sentid  depid  GDdep  (ddep dep)
----        t6 = mkTripleRef depid dependentProp (mkTokenID . d1dep $ d)
--
--
--mkDependencePart :: LanguageCode -> SentSigl -> DepSigl -> GOV_DEP -> DependencePart0 -> [Triple]
--mkDependencePart lang sentid depidp gd depp   = [t8, t9]
--    where
--       tokenid = mkTokenSigl sentid (did depp)
--       prop = case gd of
--                GDgov -> (mkRDFproperty Governor)
--                GDdep -> (mkRDFproperty Dependent)
--       t8 = mkTripleRef depidp prop tokenid
--       t9 = mkTripleLang lang depidp (mkRDFproperty  GovernorWordform)
--                                     (decode lang wf)
--       wf = word0 . dword  $ depp


----------------------------------

mkDependenceTypeTriples2 :: LanguageCode -> SentSigl ->    DependenceType0 -> [Triple]
mkDependenceTypeTriples2 lang sentid  d   =
    if  dtt d == "enhanced-plus-plus-dependencies"
    -- use only the best dependency analysis -- could filter out earlier?
            then t1 : ts
            else []
    where
        depTid = mkDepTypeSigl sentid  (dtt d)
        t1 = mkTripleType (unDepTypeSigl depTid) (mkRDFtype DepType)
        ts = concat $ map (mkDependenceTriple2 lang sentid depTid ) ( dtd d) :: [Triple]
                -- passes sentid to construct the tokenid later

--        toktrips =
----        t3 = mkTriplePartOf depid sid
--        t2 = mkTriplePartOf depid (mkSentID . d1sent $ d)
--        depsType = s2t . dt1t $ dType
--        deps2 = zip [1..] (depid dType)
--        trips = concat . map (mkDependence1Triple sid depsType) $ deps2

mkDependenceTriple2 :: LanguageCode -> SentSigl -> DepTypeSigl -> Dependence0 -> [Triple]
mkDependenceTriple2 lang sentid depTid dep   = []  -- t4 : (t5 ++ t6)
-- dependence construction produces incorrect (white space, " etc in depSigl
    where
        depid = mkDepSigl depTid (dtype dep)
--        t2oURI <#> concat ["Dependence-", mkDepID sid i]

--        t0 = mkTripleText depid  depsType

        t4 = mkTripleText (unDepSigl depid) (mkRDFproperty Dependency)  (showT $ dtype dep)
        t5 = mkDependencePart2 lang sentid  depid  GDgov  (dgov dep)
        t6 = mkDependencePart2 lang sentid  depid  GDdep  (ddep dep)
--        t6 = mkTripleRef depid dependentProp (mkTokenID . d1dep $ d)

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
--                (decode lang wf)
       wf = word0 . dword  $ depp

--decode :: LanguageCode -> Text -> Text
--decode lang wf = wf--                case lang of
--                        English -> wf
--                        German ->   wf -- s2t . latin1ToUnicode . t2s $ wf
----                        German ->   decodeLatin1 . t2b $ wf
----                        German -> fromJustNote "decode from german" . b2t . convert "Latin1" "UTF-8"  . t2b $ wf

----------------------
--mkCorefTriple :: LanguageCode -> PartURI ->  PartURI ->  Coref0 ->  [Triple]
---- ^ produce the   triples for a token  --- german only!
--mkCorefTriple lang docid snipid coref = t0 : t1 : concat  coreftrips
--    where
--        corefid = docid -- ?? TODO
--        coreftrips = map (mkMention lang docid snipid corefid) (corefMents coref)
--        t0 = mkTripleType corefid corefType
--        t1 = mkTriplePartOf corefid docid
--
--mkMention :: LanguageCode -> PartURI -> PartURI -> PartURI ->  Mention0  -> [Triple]
--mkMention lang docid snipid corefid m = [t0, t00, t1, t2, t3, t4, t5]
--    where
--        mentionid = corefid -- ?? mkMentionSigl . ment1id $ m
--        sentid = mkSentSigl  snipid (mentSent m)
--        t0 = mkTripleType mentionid mentionType
--        t00 = mkTriplePartOf mentionid corefid
--        t1 = mkTripleRef mentionid (nlpURI <#> "mentionSentence") sentid
--        t2 = mkTripleRef mentionid (nlpURI <#> "mentionSentenceStart") (mkTokenSigl sentid (mentStart $ m))
--        t3 = mkTripleRef mentionid (nlpURI <#> "mentionSentenceEnd") (mkTokenSigl sentid . mentEnd $ m)
--        t4 = mkTripleRef mentionid (nlpURI <#> "mentionSentenceHead") (mkTokenSigl  sentid . mentHead $ m)
--        t5 = mkTripleLang lang mentionid (nlpURI <#> "mentionText") ( mentText $ m)
----        TODO



--right :: Either Text a -> a
--right (Left a) = errorT ["not a right",   a]
--right (Right a) = a

--test_1_E_F_readDocString = do   -- E -> F
--    putIOwords ["test_readDocString E -> F :  "] -- tripleResult]
--    let in1 :: [Text] = map (snd . right) (result1E ::[Either Text (NLPtext, Text)])
--    t1 <- runErr $ mapM (readDocString False) in1
--    putIOwords ["test_readDocString: result  ) ", showT  t1]
----    putIOwords ["test_parseToTZ:  result ", show' t1]
--    assertEqual resutl1F_readDocStringResult t1
--
--
--
--  #include "ProduceNLP.res"
-- result1X_CD, resutl1F_readDocStringResult, result1_G_readDocCompleted, nlpTriplesResult
