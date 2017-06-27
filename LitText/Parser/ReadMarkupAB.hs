---------------------------------------------------------------------------
--
-- Module      :  Main2sub
-- Copyright   :  andrew u frank -
--
-- |  the reading the markup files and
-- removing characters not pertaining
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}

module Parser.ReadMarkupAB
    (module Parser.ReadMarkupAB
        ) where

import           Test.Framework
import Uniform.TestHarness

import           Parser.Foundation        hiding ((</>), (<.>))
import          Producer.Servers
import           Uniform.FileIO

testDir = makeAbsDir ("/home/frank/additionalSpace/DataBig/LitTest")
serverLocTest = serverBrest --


data Markup
-- just a marking for a file type

markupFileType5 = mkTypedFile5 :: TypedFile5 Text Markup

instance TypedFiles5 Text Markup  where
    -- files of a single text stream, with a markup extension
    mkTypedFile5  = TypedFile5 { tpext5 = Extension "markup"
                    -- , parserF = tparser
                    -- , writerF = twriter
            }
    write5 fp fn tp  ct = do
        dirx <- ensureDir fp
        let fn2 = fn <.> tpext5 tp -- :: Path ar File
        writeFile2 (fp </> fn2 ) ct
    read5 fp fn tp   = do
        let fn2 = fn <.> (tpext5 tp)
        readFile2 (fp </> fn2)

debugRead = False
-- main export
textstate2Text :: TextState2 -> ErrIO Text  -- test A -> B
-- reads the markup file and converts to coded llines
textstate2Text textstate = do
    t <- _readMarkupFile textstate -- test A -> B
    when debugRead $ putIOwords ["textstate2TZ - the file content\n", t,
                         "\n with show\n", showT t, "\nend of file"]
    let t2 = filterChar (`notElem` ['\r']) t
    return t2

_readMarkupFile :: TextState2 -> ErrIO Text
_readMarkupFile textstate = do
    text <-  read5 ((originalsDir $ textstate) </>
                (makeRelDir .  authorDir $ textstate) :: Path Abs Dir)

            (makeRelFile .  buchname $ textstate) markupFileType5
    bomWarning text   -- the check for BOM is in MainParse only -
    let text2 = s2t . convertLatin . t2s $ text
    let nonlats =  nubChar . findNonLatinCharsT $ text2
    unless (null' nonlats) $
                putIOwords ["this file contains characters not in the latin1"
                , " charset which are not yet mapped\n"
                 , nonlats, showT nonlats]
    return text2

bomWarning v = do  -- not required - parser filter it out
    -- putIOwords ["bomWarning - the start of the file "]
    -- putIOwords [take' 20 v]
    -- putIOwords [take' 20 $ showT v]
    if isPrefixOf' "\65279" v
        then putIOwords ["WARNING -- BOM character present - use ./unbom"]
        else putIOwords ["file start is ok"]
    return ()

test_CR :: IO ()
test_CR = assertEqual (filterChar (`notElem` ['\r']) ins) outs

  where
   ins, outs :: Text
   ins = ".sprache German\r\n.isbn ISBN -8\r\n.author Yoko Tawada\r\n.titel"
   outs = ".sprache German\n.isbn ISBN -8\n.author Yoko Tawada\n.titel"
------------------------- tests A -> B
test_1_A_B_textstate_text_1 :: IO ()
-- ^ test for the conversion from textstate to text (including markup, but not decoded)
-- the textResult is in LinesToParagraphs

testDataDir = makeAbsDir  "/home/frank/Workspace8/LitTextWorkGeras/LitTextWork/TestData"
        :: Path Abs Dir

--test_0_A_B_textstate_text_1 =   testVar2File result0A "resultB0" textstate2Text
test_1_A_B_textstate_text_1 =   testVar2File result1A "resultB1" textstate2Text
test_2_A_B_textstate_text_2 =   testVar2File result2A "resultB2" textstate2Text
test_3_A_B_textstate_text_3 =   testVar2File result3A "resultB3" textstate2Text
test_4_A_B_textstate_text_4 =   testVar2File result4A "resultB4" textstate2Text
test_5_A_B_textstate_text_5 =   testVar2File result5A "resultB5" textstate2Text
test_6_A_B_textstate_text_6 =   testVar2File result6A "resultB6" textstate2Text
test_8_A_B_textstate_text_8 =   testVar2File result8A "resultB8" textstate2Text



litTestDir1 = makeAbsDir "/home/frank/additionalSpace/DataBig/LitTest"
sourceTest = TextSource {server = serverBrest, sourceDir = litTestDir1}
destinationTest = DGoutDir litTestDir1

result1A = fillTextState2 sourceTest destinationTest "test" "t1"
result2A = fillTextState2 sourceTest destinationTest "test" "t2"
result3A = fillTextState2 sourceTest destinationTest "test" "t3"
result4A = fillTextState2 sourceTest destinationTest "test" "t4"
result5A = fillTextState2 sourceTest destinationTest "test" "t5"
result6A = fillTextState2 sourceTest destinationTest "test" "t6"
result7A = fillTextState2 sourceTest destinationTest "test" "t6"  --same
result8A = fillTextState2 sourceTest destinationTest "test" "t8"

