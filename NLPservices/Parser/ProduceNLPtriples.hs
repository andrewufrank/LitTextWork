-----------------------------------------------------------------------------
--
-- Module      :  Parser . Produce NLP triples
-- Copyright   :  andrew u frank -
--
-- | produce the triples for the NLP part
-- the analysis is completed and stored in doc0
-- this is only producing the triples, converted from doc0

-- the snip has an id (which is the paraid of the first paragraph
-- snips are not separated in paragraphs but the sentences are part of the snip and the snip part of the book



-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}

module Parser.ProduceNLPtriples
    (module Parser.ProduceNLPtriples
    , module CoreNLP.Defs0
    , module Parser.NLPvocabulary
    ) where

import           Test.Framework
import Uniform.TestHarness (testVar3File)
import CoreNLP.Defs0
import CoreNLP.NERcodes
import Parser.TextDescriptor
import NLP.Types.Tags
import Parser.NLPvocabulary  -- from Foundation
import Parser.LanguageTypedText
import Data.List (partition)
-- import Parser.ReadMarkupAB -- is in LitText, which is above NLPservices

newtype NLPtriple postag = NLPtriple Triple deriving (Eq, Ord, Show, Read)
unNLPtriple (NLPtriple t) = t


-- | a single language piece of text with lanuage code, length and start para number
data Snip2 lang = Snip2 { snip2text :: LTtext lang
                        , snip2sigl :: SnipSigl  -- the id of the snip
                          }
            deriving (Read, Show, Eq)
instance Zeros (Snip2 lang) where
    zero = Snip2 zero zero

snipIsNull :: Snip2 lang -> Bool
-- ^ test for null text
snipIsNull = null' . unLCtext . snip2text

processDoc0toTriples2 :: (Show postag, POStags postag, LanguageTypedText lang)
            => lang ->  postag -> Snip2 lang  -> Doc0 postag -> [NLPtriple postag]
            -- TriplesGraph  G -> H
-- ^ convert the doc0 (which is the analysed xml) and produce the triples

processDoc0toTriples2  lph pph snip  doc0 = map NLPtriple $ t2  : sents ++ corefs

    where
        lang = languageCode lph -- tz3lang ntz
        snipid = snip2sigl snip -- mkSnipSigl paraid snipnr
        t2 = mkTripleText (unSnipSigl snipid) (mkRDFproperty LanguageTag) (showT lang)
        sents :: [Triple]
        sents =   concat $ map (mkSentenceTriple2 lang  snipid) (docSents doc0)
        corefs = concat $ zipWith (mkCorefTriple2 lang   snipid )
                            (docCorefs doc0) [1 .. ]

----------------------
mkSentenceTriple2 :: (Show postag, POStags postag) =>
        LanguageCode ->  SnipSigl  ->    Sentence0 postag ->  ( [Triple])
-- ^ produce the   triples for a sentence
mkSentenceTriple2 lang snipid sent
       =      t0 : t1 : t2:   sentenceForm : (toktrips ++ depsTrips)
    where
        sentSigl = mkSentSigl snipid (sid sent)
        t0 = mkTripleType (unSentSigl sentSigl) (mkRDFtype Sentence)
        t1 = mkTripleText   (unSentSigl sentSigl) (mkRDFproperty Parse)
                                (sparse sent)
        t2 = mkTriplePartOf (unSentSigl sentSigl) --`buchuri
                             (unSnipSigl snipid)
        depsTrips =  maybe [] (mkDependenceTypeTriples2 lang sentSigl )
                                ( sdeps sent) :: [Triple]
                                -- in CoreNLPxml only the last dependency analysis, if any, is retained
        toktrips =  concatMap (mkTokenTriple2 lang sentSigl)
                                $ (stoks sent):: [Triple]
        sentenceForm = mkTripleLang3 lang (unSentSigl sentSigl) (mkRDFproperty SentenceForm)
                    (unwords' . map (word0 . tword) . stoks $ sent )

----------------------------------------- --
mkTokenTriple2 :: (Show postag, POStags postag) =>
        LanguageCode -> SentSigl-> Token0 postag-> [Triple]
mkTokenTriple2 lang sentSigl tok =  [t0, t1,  t2a, t3,  t5] ++ t4 ++ t6 ++ t7
    -- the language code is to pass to the triple maker ! TODO
    where
        tokensigl = mkTokenSigl sentSigl (tid tok)
        t0 = mkTripleType (unTokenSigl tokensigl) (mkRDFtype Token)
        t1 = mkTriplePartOf (unTokenSigl tokensigl)     (unSentSigl sentSigl)
--        t2 = mkTripleLang lang (unTokenSigl tokensigl)
--                    (mkRDFproperty Lemma) (lemma0 $ tlemma tok)
        t2a = mkTripleLang3 lang (unTokenSigl tokensigl)
                    (mkRDFproperty Lemma3) (lemma0 $ tlemma tok)
        t3 = mkTripleText (unTokenSigl tokensigl)
                    (mkRDFproperty Pos) (fromTag . tpos $ tok)  -- use the encoding
                    -- but represent it as text, to avoid issues with $ et
        t4 = if tpos tok == tagUNK
                    then  [mkTripleText (unTokenSigl tokensigl)
                            (mkRDFproperty PosOrig) (tposOrig $ tok) ]  -- use what nlp produced
                    else []
        t5 = mkTripleLang3 lang (unTokenSigl tokensigl) (mkRDFproperty WordForm)
                ( word0 . tword $ tok)
        t6 = if tner tok ==  [O]  -- this is the default, no need to store
                then []
                else map (mkTripleText (unTokenSigl tokensigl)
                            (mkRDFproperty Ner)) (map fromNERtag  (tner tok))
        t7 = map (mkTripleText (unTokenSigl tokensigl)
                    (mkRDFproperty Speaker) . fromSpeakerTag   )
                    (tspeaker tok)
--        t8 = if tpos tok == nerUNK
--                    then  [mkTripleText (unTokenSigl tokensigl)
--                            (mkRDFproperty NERorig) (tNERorig $ tok) ]  -- use what nlp produced
--                    else []
----------------------------------

mkDependenceTypeTriples2 :: LanguageCode -> SentSigl ->    DependenceType0 -> [Triple]
mkDependenceTypeTriples2 lang sentSigl  d   =
--              t1 : t2 :
                     ts
    where
        ts = concat $ zipWith (mkDependenceTriple2 lang sentSigl  ) ( dtd d)  [1..]
                -- passes sentSigl to construct the tokenid later

mkDependenceTriple2 :: LanguageCode -> SentSigl ->  Dependence0 -> Int -> [Triple]
mkDependenceTriple2 lang sentid  dep i  =  [mkTripleRef (unTokenSigl govtokenid)
                                             (mkRDFproperty dt)
                                             (unTokenSigl deptokenid)]
-- dependence construction produces incorrect (white space, " etc in depSigl
    where
        dt = dtype dep -- the depence type

        govTok = did . dgov $ dep   -- tokenID in sentence
        depTok = did . ddep $ dep
        govtokenid = mkTokenSigl sentid govTok
        deptokenid = mkTokenSigl sentid depTok
        -- code to preserve structure of xml
        -- =  t0:  t4 : (t5 ++ t6 ++ t7)
------ dependence construction produces incorrect (white space, " etc in depSigl
----    where
------        depid = mkDepSigl depTid i
----        depid = mkDepSigl2 sentid i
----                -- must be numbered - the same code may appear twice (dtype dep)
----        dependencyCode = fromDEPtag . dtype $ dep  -- the depCode
----        t0 = mkTripleType (unDepSigl depid) (mkRDFtype Dependence)
------        t0 = mkTripleType (unSentSigl sentid) (mkRDFtype Dependence)
------        t2 = mkTriplePartOf (unDepSigl depid)     (unDepTypeSigl depTid)
----        t4 = mkTripleText (unDepSigl depid) (mkRDFproperty Dep) dependencyCode
----        t5 = mkDependencePart2 lang sentid  depid  GDgov  (dgov dep)
----        t6 = mkDependencePart2 lang sentid  depid  GDdep  (ddep dep)
----        t7 = if (dtype dep) == tagDEPunk
----                    then  [mkTripleText (unDepSigl depid)
----                            (mkRDFproperty DepOrig) (dorig $ dep) ]  -- use what nlp produced
----                    else []
----
----data GOV_DEP = GDgov | GDdep
----
----mkDependencePart2 :: LanguageCode -> SentSigl -> DepSigl -> GOV_DEP -> DependencePart0 -> [Triple]
----mkDependencePart2 lang sentid depidp gd depp   = [t8] -- , t9]
----    where
----       tokenid = mkTokenSigl sentid (did depp)
----       prop = case gd of
----                GDgov -> (mkRDFproperty Governor)
----                GDdep -> (mkRDFproperty Dependent)
----       t8 = mkTripleRef (unDepSigl depidp) prop (unTokenSigl tokenid)
------       t9 = mkTripleLang lang (unDepSigl depidp) (mkRDFproperty DepWordform) wf
------       wf = word0 . dword  $ depp

------------ coreferences ---------------------

mkCorefTriple2 :: LanguageCode -> SnipSigl ->     Coref0 -> CorefNr ->  [Triple]
-- ^ gives a single set of coreferences  - int is to produce id -- not required
-- ^ produces triples from the mention to the representative one

mkCorefTriple2 lang snip coref i  = if null mentions then []
                                        else map (mkRefs repSigl) mentSigl
    where

            mentions = corefMents coref

            (rep,norep) = partition mentRep mentions

            rep' = case length rep of
                1 -> headNote "mkCorefTriple2 rep not present" rep :: Mention0
                _ -> errorT ["mkCoreTriple2 - mentions exist, but not a single true rep",
                        showT mentions, showT rep]
            heads = map mentHead norep
                    -- the heads are the references

            repSigl = mkRefSigl snip rep' :: TokenSigl
            mentSigl = map (mkRefSigl snip) norep  :: [TokenSigl]


mkRefSigl :: SnipSigl -> Mention0 -> TokenSigl
mkRefSigl s m = mkTokenSigl  sentid . mentHead $ m
    where       sentid = mkSentSigl  s  (mentSent m)

mkRefs :: TokenSigl -> TokenSigl -> Triple
mkRefs h m = mkTripleRef (unTokenSigl m) (mkRDFproperty Mentions) (unTokenSigl h)
        -- a is a mention of h


--mkCorefTriple2 :: LanguageCode -> SnipSigl ->     Coref0 -> CorefNr ->  [Triple]
---- ^ gives a single set of coreferences  - int is to produce id
--mkCorefTriple2 lang snip coref i  = t0 : t1 : concat  coreftrips
--    where
--        corefid = mkCorefsigl snip i
--        coreftrips = zipWith (mkMention2 lang snip  corefid) (corefMents coref) [1 .. ]
--        t0 = mkTripleType (unCorefSigl corefid) (mkRDFtype Coreference )
--        t1 = mkTriplePartOf (unCorefSigl corefid) (unSnipSigl snip)  -- perhaps not necessary
--
--mkMention2 :: LanguageCode -> SnipSigl ->   CorefSigl ->  Mention0  -> Int -> [Triple]
--mkMention2 lang snipid corefsigl m i = [t0, t10, t1, t2, t3, t4, t5]
--    where
--        mentionid = unMentionSigl $ mkMentionsigl corefsigl i
--        sentid = mkSentSigl  snipid (mentSent m)
--        t0 = mkTripleType mentionid (mkRDFtype Mention)
----        t00 = mkTriplePartOf mentionid (unCorefSigl corefsigl)
--        t10 = mkTripleText  mentionid (mkRDFproperty MentionRepresenatative)
--                    ( showT . mentRep $ m)
--                    -- true for the representative mention - i.e. not a pronoun
--        t1 = mkTripleRef mentionid (mkRDFproperty MentionSentence) (unSentSigl sentid)
--        t2 = mkTripleRef mentionid (mkRDFproperty MentionSentenceStart)
--                                    (unTokenSigl . mkTokenSigl sentid . mentStart $ m)
--        t3 = mkTripleRef mentionid (mkRDFproperty MentionSentenceEnd)
--                                    (unTokenSigl . mkTokenSigl sentid . mentEnd $ m)
--        t4 = mkTripleRef mentionid (mkRDFproperty MentionSentenceHead)
--                                    (unTokenSigl . mkTokenSigl  sentid . mentHead $ m)
--        t5 = mkTripleLang3 lang mentionid (mkRDFproperty MentionSentenceText)
--                                    ( mentText $ m)



