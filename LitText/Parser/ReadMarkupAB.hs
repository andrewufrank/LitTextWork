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

import           Parser.TextDescriptor        hiding ((</>), (<.>))
import          Producer.Servers
import           Uniform.FileIO
import Uniform.TestHarnessUtilities.Utils

--testDir = makeAbsDir ("/home/frank/additionalSpace/DataBig/LitTest")
--serverLocTest = serverBrest --


data Markup
-- just a marking for a file type

markupFileType5 = makeTyped (Extension "markup") :: TypedFile5 Text Markup

instance TypedFiles5 Text Markup  where
    -- files of a single text stream, with a markup extension
--    mkTypedFile5  = TypedFile5 { tpext5 = Extension "markup"
--                    -- , parserF = tparser
--                    -- , writerF = twriter
--            }
--    write5 fp fn tp  ct = do
--        dirx <- ensureDir fp
--        let fn2 = fn <.> tpext5 tp -- :: Path ar File
--        writeFile2 (fp </> fn2 ) ct
    read5 fp fn tp   = do
        let fn2 = fn <.> (tpext5 tp)
        readFile2 (fp </> fn2)
    read6 fp tp   =  readFile2 (fp <.> tpext5 tp)
    write6 fp   tp  ct = do
--        dirx <- ensureDir fp
        let fp2 = fp <.> tpext5 tp -- :: Path ar File
        writeFile2 fp2 ct

debugRead = False
-- main export
textstate2Text :: TextDescriptor -> ErrIO Text  -- test A -> B
-- reads the markup file and converts to coded llines
textstate2Text textstate = do
    t <- readMarkupFile textstate -- test A -> B
    when debugRead $ putIOwords ["textstate2TZ - the file content\n", t,
                         "\n with show\n", showT t, "\nend of file"]
    let t2 = filterChar (`notElem` ['\r']) t
    return t2

readMarkupFile :: TextDescriptor -> ErrIO Text
readMarkupFile textstate = do
    text <- read6 (sourceMarkup textstate) markupFileType5
--    text <-  read5 ((originalsDir $ textstate) </>
--                (makeRelDir .  authorDir $ textstate) :: Path Abs Dir)
--
--            (makeRelFile .  buchname $ textstate) markupFileType5
    bomWarning text   -- the check for BOM is in MainParse only -
    let text2 = s2t . filter (/='\SUB') . convertLatin . t2s $ text
    -- to remove the non-latin characters which were not mapped
    --    and the \sub character if any present
    let nonlats =  nubChar . findNonLatinCharsT $ text2
    unless (null' nonlats) $
                putIOwords ["this file contains characters not in the latin1"
                , " charset which are not yet mapped\n"
                 , nonlats, showT nonlats]
    return text2
bomWarning :: Text -> ErrIO ()
bomWarning v = do  -- not required - parser filter it out
    -- putIOwords ["bomWarning - the start of the file "]
    -- putIOwords [take' 20 v]
    -- putIOwords [take' 20 $ showT v]
    when (isPrefixOf' "\65279" v) $
        putIOwords ["WARNING -- BOM character present - use ./unbom"
                    , "possibly already removed in the parsing"]
    return ()

--test_CR :: IO ()
--test_CR = assertEqual (filterChar (`notElem` ['\r']) ins) outs
--
--  where
--   ins, outs :: Text
--   ins = ".sprache German\r\n.isbn ISBN -8\r\n.author Yoko Tawada\r\n.titel"
--   outs = ".sprache German\n.isbn ISBN -8\n.author Yoko Tawada\n.titel"
--------------------------- tests A -> B
--test_1_A_B_textstate_text_1 :: IO ()
---- ^ test for the conversion from textstate to text (including markup, but not decoded)
---- the textResult is in LinesToParagraphs

--testDataDir = makeAbsDir  "/home/frank/Workspace8/LitTextWorkGeras/LitTextWork/TestData"
--        :: Path Abs Dir

------test_0_A_B_textstate_text_1 =   testVar2File result0A "resultB0" textstate2Text
test_1_A_B_textstate_text_1 =   testVar2File result1A "resultB1" textstate2Text
test_2_A_B_textstate_text_2 =   testVar2File result2A "resultB2" textstate2Text
test_3_A_B_textstate_text_3 =   testVar2File result3A "resultB3" textstate2Text
test_4_A_B_textstate_text_4 =   testVar2File result4A "resultB4" textstate2Text
test_5_A_B_textstate_text_5 =   testVar2File result5A "resultB5" textstate2Text
test_6_A_B_textstate_text_6 =   testVar2File result6A "resultB6" textstate2Text
test_8_A_B_textstate_text_8 =   testVar2File result8A "resultB8" textstate2Text
test_9_A_B_textstate_text_9 =   testVar2File result9A "resultB9" textstate2Text
test_10_A_B_textstate_text_10 =   testVar2File result10A "resultB10" textstate2Text
test_11_A_B_textstate_text_11 =   testVar2File result11A "resultB11" textstate2Text
test_12_A_B_textstate_text_12 =   testVar2File result12A "resultB12" textstate2Text



--litTestDir1 = makeAbsDir "/home/frank/additionalSpace/DataBig/LitTest"
--sourceTest = TextSource {server = serverBrest, sourceDir = litTestDir1}
--destinationTest = DGoutDir litTestDir1
fill_ :: FilePath -> FilePath -> TextDescriptor

--fill_ f1 f2 = fillTextState3a dirsTest serverBrest f1 f2 False  -- 3a not including text
fill_ f1 f2 = fillTextState3a dirsTest serverLocalhost f1 f2 False  -- 3a not including text
result1A = fill_ "test" "t1"
result2A = fill_ "test" "t2"
result3A = fill_ "test" "t3"
result4A = fill_ "test" "t4"
result5A = fill_ "test" "t5"
result6A = fill_ "test" "t6"
result7A = fill_ "test" "t6"  --same
result8A = fill_ "test" "t8"
result9A = fill_ "test" "t9"
result10A = fill_ "test" "t10"
result11A = fill_ "test" "t11"  -- italian character set issues
result12A = fill_ "test" "t12"  -- italian character set issues

