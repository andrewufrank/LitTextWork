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
import Process.UtilsParseArgs ( LitTextFlags (..) )

mainLitAndNLPproduction :: LitTextFlags -> TextDescriptor -> ErrIO ()
mainLitAndNLPproduction flags  textstate = do
    let debugLit = flagDebug flags
    when debugLit $ putIOwords ["mainLitAndNLPproduction start", showT textstate]
    --- convert to TextZeilen format
    ttext <- textstate2Text textstate -- test A - B (in this module)
    let ttzeilen = parseMarkup ttext   -- test B -> BA in BuchCode.MarkupText
    let tzlayout = paragraphs2TZlayout ttzeilen ::  [TZ]
    let tzlayout1 = paragraphs2TZsimple tzlayout :: [TZ1]
        -- ignore line, allCaps, language
        -- missing footnotes?
    let layoutTriples = produceLayoutTriples textstate tzlayout1  -- BAD -> J

    when debugLit $ putIOwords ["mainLitAndNLPproduction layout triples done \n"
                            , unlines' . map showT $ layoutTriples]
    textstate2 <- if includeText textstate
                then do
                    nt2 <-  writeHandleTriples (ntdescriptor textstate) layoutTriples
                    return (textstate{ntdescriptor = nt2})
                else return textstate

    let tzpara = paragraphsTZ2TZ2  tzlayout1     -- test BAD -> BAE   in LinesToParagraph

    when debugLit $ putIOwords
            ["mainLitAndNLPproduction TZ available to produce litTriples \n"
            , unlines' . map showT $ tzpara]

    let litTriples = produceLitTriples textstate2   tzpara  -- test BAE=C -> H and K (nt)

    when debugLit $  putIOwords ["triples \n", unlines' . map showT $ litTriples]

    ntdesc3 <- writeHandleTriples (ntdescriptor textstate2) litTriples
    let textstate3 = textstate2{ntdescriptor = ntdesc3}
    --        if includeText textstate
--                                then writeHandleTriples textstate2 litTriples
--                                else return textstate2

--    writeTriples2file textstate (layoutTriples ++ litTriples)
--    when debugLit $ putIOwords ["lit: triples stored in file \n",
--             unlines' . map showT $ (layoutTriples ++ litTriples)
--                    ]

--    when (not . includeText $ textstate) $
--        throwError . unlines' $  [ "\nmainLitAndNLPproduction stopped"
--        , "\n because debugLitOnly true - set to lit only!\n"
--        , showT textstate
--        ]

    --------------------------------------NLP  -- processing by paragraphs

    responses <- produceNLP  textstate3 tzpara -- test D ->
        -- argument is to show the xml

    putIOwords ["mainLitAndNLPproduction: triples stored in .nt file "
--           , showT . graph $ textstate, " \n"
--            , unlines' . map showT $ responses
            ]
    ntstate4 <- case destHandle (ntdescriptor textstate3) of
        Nothing -> return (ntdescriptor textstate3)
        Just h -> closeHandleTriples (ntdescriptor textstate3)
    let textstate4 = textstate3 {ntdescriptor = ntstate4}
    return ()
