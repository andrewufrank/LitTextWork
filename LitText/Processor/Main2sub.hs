---------------------------------------------------------------------------
--
-- Module      :  Main2sub
-- Copyright   :  andrew u frank -
--
-- |  the common process to producing the lit and nlp triples
-- could test initially if the services (treetagger, fuseki, corenlp are available
-----------------------------------------------------------------------------
--{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}

module Processor.Main2sub (mainLitAndNLPproduction
        ) where

--import           Test.Framework
import Parser.ReadMarkupAB
--import           Lines2para.MarkupText
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
import Process.UtilsParseArgs ( LitTextFlags (..) )

mainLitAndNLPproduction :: LitTextFlags -> TextDescriptor -> ErrIO ()
mainLitAndNLPproduction flags  textstate = do
    let debugLit = flagDebug flags

    -- read text input
    when debugLit $ putIOwords ["mainLitAndNLPproduction start", showT textstate]
    ttext <- textstate2Text textstate -- test A - B (in this module)

    -- convert text to tz1 (module Lines2para - handles layout)
--    let ttzeilen = parseMarkup ttext   -- test B -> BA in BuchCode.MarkupText
--    let tzlayout = paragraphs2TZlayout ttzeilen ::  [TZ]
--    let tzlayout1 = paragraphs2TZsimple tzlayout :: [TZ1]
--        -- ignore line, allCaps, language
--        -- missing footnotes?
    let tzlayout1 = text2tz1 ttext :: [TZ1]  -- B -> C

    -- produce triples
    let layoutTriples = produceLayoutTriples textstate tzlayout1 :: [Triple] -- C -> J

    when debugLit $ putIOwords ["mainLitAndNLPproduction layout triples done \n"
                            , unlines' . map showT $ layoutTriples]

    let tzpara = paragraphsTZ2TZ2  tzlayout1  :: [TZ2] -- test BAD -> BAE   in LinesToParagraph
    let litTriples = produceLitTriples textstate   tzpara  -- test BAE=C -> H and K (nt)

    when debugLit $  putIOwords ["triples \n", unlines' . map showT $ litTriples]

    nlpTriples <- produceNLPtriples  textstate tzpara -- test D ->

    putIOwords ["mainLitAndNLPproduction: triples stored in .nt file "
--           , showT . graph $ textstate, " \n"
--            , unlines' . map showT $ responses
            ]

    let ntdescr = ntdescriptor textstate

    bracketErrIO (openHandleTriples ntdescr )
                (closeHandleTriples ntdescr )
                (\h -> do
                    writeHandleTriples ntdescr h layoutTriples
                    when (flagIncludeText flags) $ writeHandleTriples ntdescr h litTriples
                    writeHandleTriples ntdescr h nlpTriples
                    )

    return ()
