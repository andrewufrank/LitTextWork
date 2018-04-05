---------------------------------------------------------------------------
--
-- Module      :  Main2sub
-- Copyright   :  andrew u frank -
--
-- |  the common process to producing the lit and nlp triples
-- could test initially if the services (treetagger, fuseki, corenlp are available
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}

-- import the fundamental types from TextDescriptor, not from processing steps

module Processor.Main2sub (mainLitAndNLPproduction
         , ntFileTriples,ntFileTriplesGZip
--         , LitTextFlags (..), LitTextFlag (..)
            , LitTextFlags, LitTextFlags (..)
         , TextDescriptor (..)
         , ErrIO (..)
--         , URI (..)
            ) where

import           Parser.ReadMarkupAB (textstate2Text
--                        , LitTextFlag (..), LitTextFlags (..)
                        , LitTextFlagSet , LitTextFlags (..)
                        , TextDescriptor (..))
import           Lines2para.Lines2text  (text2tz1, TZ1)

import           Parser.ProduceLayout (produceLayoutTriples)

import           Lines2para.Lines2para (paragraphsTZ2TZ2, TZ2) -- hiding ((</>))

import           Parser.ProduceLit (produceLitTriples, Triple)
import           Parser.ProduceNLP (produceNLPtriples, tz2toSnip, Snip)


import           Uniform.FileIO -- (when, errorT)

mainLitAndNLPproduction :: LitTextFlagSet -> TextDescriptor -> ErrIO ()
mainLitAndNLPproduction flags  textstate = do
    let debugLit = isDebugFlag flags -- DebugFlag `elem` flags

    -- read text input
    when debugLit $ putIOwords ["mainLitAndNLPproduction start", showT textstate]
    -- use Parser.ReadMarkup
    ttext <- textstate2Text textstate -- test A - B (in this module)

--        -- ignore line, allCaps, language
--        -- missing footnotes?

    -- use Lines2para.Line2text
    let tzlayout1 = text2tz1 ttext :: [TZ1]  -- B -> C

    -- produce triples
    let layoutTriples = produceLayoutTriples textstate tzlayout1 :: [Triple] -- C -> J

    when debugLit $ putIOwords ["mainLitAndNLPproduction layout triples done \n"
                            , unlines' . map showT $ layoutTriples]
    -- use Lines2para.Lines2para
    let tzpara = paragraphsTZ2TZ2  tzlayout1  :: [TZ2] -- test BAD -> BAE   in LinesToParagraph

    let litTriples = produceLitTriples textstate   tzpara  -- test BAE=C -> H and K (nt)

    when debugLit $  putIOwords ["triples \n", unlines' . map showT $ litTriples]

    let snips = tz2toSnip  textstate tzpara  :: [Snip]

    nlpTriples <- produceNLPtriples flags textstate snips -- test D ->

    putIOwords ["mainLitAndNLPproduction: triples stored in .nt file "
--           , showT . graph $ textstate, " \n"
--            , unlines' . map showT $ responses
            ]

    let ntdescr = ntdescriptor textstate

    bracketErrIO (openHandleTriples ntdescr )
                (closeHandleTriples ntdescr )
                (\h -> do
                    writeHandleTriples ntdescr h layoutTriples
                    when (isIncludeText flags) $ writeHandleTriples ntdescr h litTriples
                    writeHandleTriples ntdescr h nlpTriples
                    )

    return ()
