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

module Parser.ProduceDocCallNLP_test  where

import              Test.Framework
import              Uniform.TestHarness
--import Parser.LanguageTypedText
--import Producer.Servers
--import CoreNLP.CoreNLPxml (readDocString)
--import CoreNLP.Defs0 ()
--import Uniform.HttpCallWithConduit (callHTTP10post, addPort2URI, callHTTP8get, addToURI)
--import Text.Regex (mkRegex, subRegex)
--import Parser.CompleteSentence (completeSentence)
import Parser.ProduceNLPtriples -- (processDoc0toTriples2)
import Parser.LanguageTypedText
--
--import NLP.Corpora.Conll  as Conll -- Conll for english
--import NLP.Corpora.ItalianTinT   as TinT-- for italian
--import NLP.Corpora.German  as German --
--import NLP.Corpora.Spanish as Spanish --
--import NLP.Corpora.French as French --
--import NLP.Corpora.FrenchUD as FrenchUD --
--
--import Data.Text as T b

import Parser.ProduceDocCallNLP




italianGutenberg = "Cosě, non fu contento finchč. E cosě,   han giŕ voluto che piů vi stesse a dondolare"
italianResult = "Cosi, non fu contento finche. E cosi,   han gia voluto che piu vi stesse a dondolare"

test_italGutenberg = assertEqual italianResult (cleanTextItalian italianGutenberg)

-- Cos\283, non fu contento finche. E cos\283,   han gia voluto che piu vi stesse a dondolare

entz3text = "Test Multiple Languages.   The uncle flew to Boston. When he came into the room, he carried a book. It was red."
gertz3text = "Der Onkel flog nach Boston. Als er den Raum betrat, hatte er ein Buch dabei. Es war rot."
fretz3text = "L'oncle prit l'avion pour Boston. Quand il entrat la chamre, il portait un livre. Il etait rouge."
spantz3text = "El t\237o vol\243 a Boston. Cuando entr\243 en la habitaci\243n, tra\237a un libro. Que era de color rojo."
ittz3text = "Lo zio vol\242 a Boston. Quando entr\242 nella stanza, port\242 un libro. Era rosso."

paraSigl1 =  ParaSigl ( extendSlashRDFsubj "produceDocCallNLP" (RDFsubj $ (showT rdfBase))  )

snip2eng = Snip2 (typeText undefEnglish entz3text) (mkSnipSigl paraSigl1 (SnipID 1))
--nlpserver = serverBrest



--------------------------



test_clean1 = assertEqual "The father went to the row house for 2 d  and got it."
        (cleanTextEnglish "The _father_ went to the row-house for 2d. and got it.")
test_clean2 = assertEqual "  Knots of idle men  on the South Bridge, for 3 s  2 d  .\
        \   This street named the Via Dolorosa."
 (cleanTextEnglish tx1)

tx1 = "  Knots of idle-men  \
    \on the South Bridge, for 3s. 2d. .   \
    \This street named the _Via Dolorosa_."

--------------
    -- converts a text to snip2
testOP_Snip_N (langPh, postagPh, text, i)= do
        putIOwords [ "testOP"]
        let snip  = Snip2 (typeText langPh text) (mkSnipSigl paraSigl1 (SnipID i))
        return snip
--        convertOneSnip2Triples2 langPh postagPh True snip serverBrest

--test_N_1 :: IO ()
test_M_1 = testVar2File (undefEnglish, undefConll, entz3text, 1)        "resultM1" testOP_Snip_N
test_M_2 = testVar2File (undefGerman, undefGermanPos, gertz3text, 2)    "resultM2" testOP_Snip_N
test_M_3 = testVar2File (undefFrench, undefFrenchPos, fretz3text, 3)    "resultM3" testOP_Snip_N
test_M_4 = testVar2File (undefSpanish, undefSpanishPos, spantz3text, 4) "resultM4" testOP_Snip_N
test_M_5 = testVar2File (undefItalian, undefTinTPos, ittz3text, 5)      "resultM5" testOP_Snip_N

instance  ShowTestHarness (Snip2 a) where
    -- to avoid the additional "" added when show  text
    showTestH = show
    readTestH = readNote "showTestHarness Snip2"

testOP_M_N (langPh, postagPh, text, i) snip =
        convertOneSnip2Triples2 langPh postagPh True snip serverBrest

----test_N_1 :: IO ()
test_N_1 = testVar3File (undefEnglish, undefConll, entz3text, 1) "resultM1" "resultN1" testOP_M_N
--test_N_2 = testVar2File (undefGerman, undefGermanPos, gertz3text, 2)    "resultN2" testOP_M_N
--test_N_3 = testVar2File (undefFrench, undefFrenchPos, fretz3text, 3)    "resultN3" testOP_M_N
--test_N_4 = testVar2File (undefSpanish, undefSpanishPos, spantz3text, 4) "resultN4" testOP_M_N
--test_N_5 = testVar2File (undefItalian, undefTinTPos, ittz3text, 5)      "resultN5" testOP_M_N

