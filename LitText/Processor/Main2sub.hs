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

module Processor.Main2sub (mainLitAndNLPproduction
    , htf_thisModulesTests
        ) where

import           Test.Framework
import Parser.ReadMarkupAB
import           BuchCode.MarkupText
import Parser.ProduceLayout
import           Lines2para.Lines2para hiding ((</>))
import           Lines2para.Lines2ignore
import           Parser.ProduceLit
import           Parser.ProduceNLP
import           Uniform.FileIO (when, errorT)
import           Uniform.Strings
import Lines2para.HandleLayout
import Data.RDF.FileTypes (ntFileTriples)
-- (parseMarkup, result1B, result2B, result3B, result4B)

mainLitAndNLPproduction :: Bool -> Bool -> TextDescriptor -> ErrIO ()
mainLitAndNLPproduction debugLit produceLitOnly textstate = do
    when debugLit $ putIOwords ["mainLitAndNLPproduction start", showT textstate]
    --- convert to TextZeilen format
    ttext <- textstate2Text textstate -- test A - B (in this module)
    let ttzeilen = parseMarkup ttext   -- test B -> BA in BuchCode.MarkupText
    let tzlayout = paragraphs2TZlayout ttzeilen ::  [TZ]
    let tzlayout1 = paragraphs2TZsimple tzlayout :: [TZ]
        -- ignore line, allCaps, language
        -- missing footnotes?
    let layoutTriples = produceLayoutTriples textstate tzlayout1  -- BAD -> J

    when debugLit $ putIOwords ["mainLitAndNLPproduction layout triples done \n"
                            , unlines' . map showT $ layoutTriples]
    textstate2 <- if includeText textstate
                then  writeHandleTriples textstate layoutTriples
                else return textstate

    let tzpara = paragraphsTZ2TZ2  tzlayout1     -- test BAD -> BAE   in LinesToParagraph

    when debugLit $ putIOwords
            ["mainLitAndNLPproduction TZ available to produce litTriples \n"
            , unlines' . map showT $ tzpara]

    let litTriples = produceLitTriples textstate2   tzpara  -- test BAE=C -> H and K (nt)

    when debugLit $  putIOwords ["triples \n", unlines' . map showT $ litTriples]

    textstate3 <- writeHandleTriples textstate2 litTriples
--        if includeText textstate
--                                then writeHandleTriples textstate2 litTriples
--                                else return textstate2

--    writeTriples2file textstate (layoutTriples ++ litTriples)
--    when debugLit $ putIOwords ["lit: triples stored in file \n",
--             unlines' . map showT $ (layoutTriples ++ litTriples)
--                    ]

    when produceLitOnly $
        throwError . unlines' $  [ "\nmainLitAndNLPproduction stopped"
        , "\n because debugLitOnly true - set to lit only!\n"
        , showT textstate
        ]

    --------------------------------------NLP  -- processing by paragraphs

    responses <- produceNLP False textstate3 tzpara -- test D ->
        -- argument is to show the xml

    putIOwords ["mainLitAndNLPproduction: triples stored in .nt file "
--           , showT . graph $ textstate, " \n"
--            , unlines' . map showT $ responses
            ]
    textstate4 <- case destHandle textstate3 of
        Nothing -> return textstate3
        Just h -> closeHandleTriples textstate3
    return ()
