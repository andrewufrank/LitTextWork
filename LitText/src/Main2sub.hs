---------------------------------------------------------------------------
--
-- Module      :  Main2sub
-- Copyright   :  andrew u frank -
--
-- |  the common process to producing the lit and nlp triples
-- could test initially if the services (treetagger, fuseki, corenlp are available
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}

module Main2sub (mainLitAndNLPproduction
    , htf_thisModulesTests
        ) where

import           Test.Framework

import Parser.Main2subTest
-- immediate under this, imports everything here imported
import Parser.ReadMarkupAB
import           BuchCode.MarkupText
--import           Parser.Foundation        hiding ((</>))
import           Lines2para.Lines2para hiding ((</>))
import           Parser.ProduceLit
--import           Parser.ProduceDocCallNLP
import           Parser.ProduceNLPtriples
--import           Store.Fuseki
import           Uniform.FileIO (when, errorT)
import           Uniform.Strings
-- (parseMarkup, result1B, result2B, result3B, result4B)

debugNLP = False
debugLit = False
mainLitAndNLPproduction :: Bool -> TextState2 -> ErrIO ()
mainLitAndNLPproduction debugLitonly textstate = do
    --- convert to TextZeilen format
    ttext <- textstate2Text textstate -- test A - B (in this module)
    let ttzeilen = parseMarkup ttext   -- test B -> BA in BuchCode.MarkupText
    let tzpara = paragraphs2TZ  ttzeilen     -- test BA -> C  in LinesToParagraph
--    let tzpara = (paragraphs2TZpara . paragraphs2TZsimple . paragraphs2TZlayout) ttzeilen     -- test BA -> C  in LinesToParagraph

--    tzpara <- textstate2TZ textstate  -- test A -> C

    when debugLit $ putIOwords ["TZ available to produce trips \n", unlines' . map showT $ tzpara]

    let trips = produceLitTriples textstate   tzpara  -- test C -> H

    when debugLit $  putIOwords ["triples \n", unlines' . map showT $ trips]

    writeTriples2file textstate trips
--    response <- storeTriplesFuseki  textstate trips  -- test H -> Z

    when debugLit $ putIOwords ["lit: triples stored with fuseki in graph "
--                    , showT . graph $  textstate
--                    , " \n", showT response
                    ]

    when debugLitonly $  errorT [ "MainLit stopped because debugLitOnly true - set to lit only!"
            , showT textstate]

    --------------------------------------NLP  -- processing by paragraphs
    -- putIOwords ["NLP conversion", "start" ]
--    let tzparaText = prepareTZ4nlp tzpara

    responses <- produceNLPtriples textstate tzpara -- test D ->

    putIOwords ["npl: triples stored with fuseki in graph (responses and para/seite) "
--           , showT . graph $ textstate, " \n"
--            , unlines' . map showT $ responses
            ]

    return ()
