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
import Parser.ReadMarkupAB
import           BuchCode.MarkupText
import           Lines2para.Lines2para hiding ((</>))
import           Parser.ProduceLit
import           Parser.ProduceNLP
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
    let tzlayout = paragraphs2TZlayout ttzeilen

    let tzpara = paragraphsTZ2TZ2  tzlayout     -- test BA -> C  in LinesToParagraph

    when debugLit $ putIOwords ["TZ available to produce trips \n", unlines' . map showT $ tzpara]

    let trips = produceLitTriples textstate   tzpara  -- test C -> H

    when debugLit $  putIOwords ["triples \n", unlines' . map showT $ trips]

    writeTriples2file textstate trips
    when debugLit $ putIOwords ["lit: triples stored with fuseki in graph "
                    ]

    when debugLitonly $  errorT [ "MainLit stopped because debugLitOnly true - set to lit only!"
            , showT textstate]

    --------------------------------------NLP  -- processing by paragraphs
    -- putIOwords ["NLP conversion", "start" ]
--    let tzparaText = prepareTZ4nlp tzpara

    responses <- produceNLP textstate tzpara -- test D ->

    putIOwords ["npl: triples stored with fuseki in graph (responses and para/seite) "
--           , showT . graph $ textstate, " \n"
--            , unlines' . map showT $ responses
            ]

    return ()
