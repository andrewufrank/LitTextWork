 -----------------------------------------------------------------------------
--
-- Module      :  Parser . ProduceDocCallNLP  -- BAE=C -> D
-- Copyright   :  andrew u frank -
--
-- | convert the whole text to the selection which will be NLP analyzed

-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables
    ,TypeSynonymInstances
    , MultiParamTypeClasses
    , NoMonomorphismRestriction
    , UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}

module NLP2RDF.ProduceDocCallNLP_test  where

import              Test.Framework
import              Uniform.Test.TestHarness
--import CoreNLP.ProduceNLPtriples -- (processDoc0toTriples2, Snip2 (..), Doc0 (..))
import CoreNLP.Doc2ToLinear
import CoreNLP.CoreNLP (NTtext (..), unNT)
--import LitTypes.LanguageTypedText
import CoreNLP.Vocabulary

import NLP2RDF.ProduceDocCallNLP
import NLP.TagSets.Conll  as Conll -- Conll for english
import NLP.TagSets.German  as German --
import NLP.TagSets.Spanish as Spanish --
import NLP.TagSets.French as French --
import NLP.TagSets.FrenchUD as FrenchUD --
import NLP.TagSets.ItalianTinT   as TinT-- for italian
import NLP.TagSets.UD as UD --

progName = "nlpservices"

italianGutenberg = "Cosě, non fu contento finchč. E cosě,   \
    \han giŕ voluto che piů vi stesse a dondolare"
italianResult = "Cosi, non fu contento finche. E cosi,   \
    \han gia voluto che piu vi stesse a dondolare"

-- test_italGutenberg = assertEqual italianResult (cleanTextItalian italianGutenberg)

-- Cos\283, non fu contento finche. E cos\283,
--        han gia voluto che piu vi stesse a dondolare

entz3text = "Test Multiple Languages.   The uncle flew to Boston. \
    \When he came into the room, he carried a book. It was red."
gertz3text = "Der Onkel flog nach Boston. Als er den Raum betrat, \
    \hatte er ein Buch dabei. Es war rot."
fretz3text = "L'oncle prit l'avion pour Boston. Quand il entrat la chamre, \
    \il portait un livre. Il etait rouge."
spantz3text = "El t\237o vol\243 a Boston. Cuando entr\243 en la \
    \habitaci\243n, tra\237a un libro. Que era de color rojo."
ittz3text = "Lo zio vol\242 a Boston. Quando entr\242 nella stanza, \
    \port\242 un libro. Era rosso."

paraSigl1 =  ParaSigl ( extendSlashRDFsubj "produceDocCallNLP"
                            (RDFsubj $ (unPartURI rdfBase))  )

data Snip2 lang = Snip2 (LTtext lang) RDFsubj
    deriving (Eq, Ord, Read, Show)
    -- the data type Snip2 is not used anymore - here just for testing
instance Zeros (Snip2 lang) where zero = Snip2 zero zero

--instance Read (Snip2 lang) where

snip2eng = (typeText undefEnglish entz3text, rdfBase)
--                (mkSnipSigl paraSigl1 (SnipID 1))
nlpserver = serverBrest

--sigl1 = mkSnipSigl paraSigl1 (SnipID 1)
--test_1 = do
--        putIOwords ["show sigl1", s2t $ show sigl1]
--        putIOwords ["show parasigl1", s2t $ show paraSigl1]
--        assertBool False

--test_readSigl = assertEqual sigl1 (read . show $ sigl1)

--------------------------



--test_clean1 = assertEqual
--    "The father went to the row house for 2 d  and got it."
--   (cleanTextEnglish "The _father_ went to the row-house for 2d. and got it.")
--test_clean2 = assertEqual
--    "  Knots of idle men  on the South Bridge, for 3 s  2 d  .\
--        \   This street named the Via Dolorosa."
--  (cleanTextEnglish tx1)

tx1 = "  Knots of idle-men  \
    \on the South Bridge, for 3s. 2d. .   \
    \This street named the _Via Dolorosa_."

instance Zeros PartURI where zero = PartURI ""

-- the vars (always the same
englVars = (undefEnglish, Conll.undefPOS, entz3text, 1)
germanVars = (undefGerman, German.undefPOS, gertz3text, 2)
frenchVars = (undefFrench, French.undefPOS, fretz3text, 3)
spanishVars = (undefSpanish, UD.undefPOS, spantz3text, 4)
-- spanish wit json format seems to produce UD
italianVars = (undefItalian, TinT.undefPOS, ittz3text, 5)
udfeatsVars = (undefEnglish, UD.undefPOS, entz3text, 1)
--------------
    -- converts a text to snip2
testOP_Snip_M :: LanguageTypedText t0 =>
        (t0, t1, Text, Int) -> ErrIO (Snip2 t0)
testOP_Snip_M (langPh, postagPh, text, i)= do
        putIOwords [ "testOP"]
        let snip  = Snip2 (typeText langPh text)
                         (RDFsubj . unPartURI $ rdfBase)  -- )
--                        (mkSnipSigl paraSigl1 (SnipID i))
        return snip

 -- M is just the snip
test_M_1 :: IO ()
test_M_1 = testVar0FileIO progName englVars "resultM1" testOP_Snip_M
test_M_2 = testVar0FileIO progName germanVars "resultM2" testOP_Snip_M
test_M_3 = testVar0FileIO progName frenchVars "resultM3" testOP_Snip_M
test_M_4 = testVar0FileIO progName spanishVars "resultM4" testOP_Snip_M
test_M_5 = testVar0FileIO progName italianVars "resultM5" testOP_Snip_M
test_M_6 = testVar0FileIO progName udfeatsVars "resultM6" testOP_Snip_M

instance  ShowTestHarness (Snip2 a) where
    -- to avoid the additional "" added when show  text
--    showTestH = show
--    readTestH2 = readNote -- "showTestHarness Snip2"

instance  ShowTestHarness (Doc1 a) where
    -- to avoid the additional "" added when show  text
--    showTestH = show
--    readTestH2 = readNote -- "showTestHarness Doc1"

instance ShowTestHarness NTtext where

testOP_M_N :: (-- TaggedTyped t1,
        LanguageTypedText t0, LanguageTyped2 t0 t1) =>
         (t0, t1, Text, Int) -> Snip2 t0 -> ErrIO Text
testOP_M_N (langPh, postagPh, text, i) (Snip2 txt base)   = do
         res <- snip2triples langPh postagPh [DebugFlag] txt base serverBrest
         let res2 = map triple2text res
         return . unlines' $ res2

        -- overall test, snip to triples

--testVar3File :: (Read a, Eq b, Show b, Read b
--            , Zeros b, ShowTestHarness b) =>
--        base -> FilePath -> FilePath -> (base -> a->   b) -> IO ()
test_M_N1 :: IO ()
test_M_N1 = testVar1FileIO progName englVars "resultM1" "resultN1" testOP_M_N
test_M_N2 = testVar1FileIO progName germanVars "resultM2" "resultN2" testOP_M_N
test_M_N3 = testVar1FileIO progName frenchVars "resultM3" "resultN3" testOP_M_N
test_M_N4 = testVar1FileIO progName spanishVars "resultM4" "resultN4" testOP_M_N
test_M_N5 = testVar1FileIO progName italianVars "resultM5" "resultN5" testOP_M_N
test_M_N6 = testVar1FileIO progName udfeatsVars "resultM6" "resultN6" testOP_M_N

----    text2xml :: postag -> Bool -> URI -> Text
--    -> [(Text,Maybe Text)] -> Text
--                    ->  ErrIO Text

-- produce the json returned
testOP_M_MA :: (
--     (Show postag, TaggedTyped postag, LanguageTypedText lang
                  LanguageTyped2 lang postag) =>
         (lang, postag, Text, Int) -> Snip2 lang -> ErrIO Text
testOP_M_MA (langPh, postagPh, _, _) (Snip2 txt base) = do
            json1 <- text2nlpCode   True server  path
                        vars (unLCtext txt)
            -- add a json pretty print here?
            return json1
        where   vars = nlpParams langPh postagPh
                path = nlpPath langPh
                server = addPort2URI serverBrest (nlpPort langPh postagPh)

test_M_MA1 :: IO ()
test_M_MA1 = testVar1FileIO progName englVars "resultM1" "resultMA1" testOP_M_MA
test_M_MA2 = testVar1FileIO progName germanVars "resultM2" "resultMA2" testOP_M_MA
test_M_MA3 = testVar1FileIO progName frenchVars "resultM3" "resultMA3" testOP_M_MA
test_M_MA4 = testVar1FileIO progName spanishVars "resultM4" "resultMA4" testOP_M_MA
test_M_MA5 = testVar1FileIO progName italianVars "resultM5" "resultMA5" testOP_M_MA
--fails always, because it contains timing info
test_M_MA6 = testVar1FileIO progName udfeatsVars "resultM6" "resultMA6" testOP_M_MA




    -- produce NT from whatever nlp produces (xml, json, conllu)
testOP_M_MB ::  (Show postag
--            , TaggedTyped postag
            , LanguageTypedText lang
                , LanguageTyped2 lang postag) =>
         (lang, postag, Text, Int) -> Text ->  Text
testOP_M_MB (langPh, postagPh, text, i)   =
            unlines' . map triple2text
             . postNLP  postagPh langPh True
                    (RDFsubj . unPartURI $ rdfBase)

test_M_MB1 :: IO ()
test_M_MB1 = testVar1File progName englVars "resultMA1" "resultMB1" testOP_M_MB
test_M_MB2 = testVar1File progName germanVars  "resultMA2" "resultMB2" testOP_M_MB
test_M_MB3 = testVar1File progName frenchVars "resultMA3" "resultMB3" testOP_M_MB
test_M_MB4 = testVar1File progName spanishVars "resultMA4" "resultMB4" testOP_M_MB
test_M_MB5 = testVar1File progName italianVars "resultMA5" "resultMB5" testOP_M_MB
test_M_MB6 = testVar1File progName udfeatsVars "resultMA6" "resultMB6" testOP_M_MB


----- next test ?

--testOP_MA_MC :: (Show postag, TaggedTyped postag, LanguageTypedText t0
--        , LanguageTyped2 t0 postag) =>
--            (t0, postag, Text, Int) -> Text -> ErrIO (Doc1 postag)
--
--testOP_MA_MC (langPh, postagPh, text, i) xml1 = do
--        nlpCode2doc1   postagPh False  xml1
--
--test_MA_MC_1 = testVar3FileIO (undefEnglish, undefConll, entz3text, 1)
--                         "resultMA1" "resultMC1" testOP_MA_MC
--test_M_MC2 = testVar3FileIO (undefGerman, undefGermanPos, gertz3text, 2)
--                                 "resultMA2" "resultMC2" testOP_MA_MC
--test_M_MC3 = testVar3FileIO (undefFrench, undefFrenchPos, fretz3text, 3)
--                           "resultMA3" "resultMC3" testOP_MA_MC
--test_M_MC4 = testVar3FileIO (undefSpanish, undefSpanishPos, spantz3text, 4)
--                                  "resultMA4" "resultMC4" testOP_MA_MC
--test_MA_MC_5 = testVar3FileIO (undefItalian, undefTinTPos, ittz3text, 5)
--                                 "resultMA5" "resultMC5" testOP_MA_MC


