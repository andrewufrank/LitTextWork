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

module Parser.ReadMarkupAB_test where

import           Test.Framework
import Uniform.Test.TestHarness

import           LitText.Foundation
--        hiding ((</>), (<.>))
import           Uniform.FileIO
import Parser.ReadMarkupAB

progName = "tests"
--instance ShowTestHarness [TextZeile]

--testDir = makeAbsDir ("/home/frank/additionalSpace/DataBig/LitTest")
--serverLocTest = serverBrest --



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

------test_0_A_B_textstate_text_1 =   testVar0FileIO progName result0A "resultB0" textstate2Text
test_1_A_B_textstate_text_1 =   testVar0FileIO progName result1A "resultB1" textstate2Text
test_2_A_B_textstate_text_2 =   testVar0FileIO progName result2A "resultB2" textstate2Text
test_3_A_B_textstate_text_3 =   testVar0FileIO progName result3A "resultB3" textstate2Text
test_4_A_B_textstate_text_4 =   testVar0FileIO progName result4A "resultB4" textstate2Text
test_5_A_B_textstate_text_5 =   testVar0FileIO progName result5A "resultB5" textstate2Text
test_6_A_B_textstate_text_6 =   testVar0FileIO progName result6A "resultB6" textstate2Text
test_8_A_B_textstate_text_8 =   testVar0FileIO progName result8A "resultB8" textstate2Text
test_9_A_B_textstate_text_9 =   testVar0FileIO progName result9A "resultB9" textstate2Text
test_10_A_B_textstate_text_10 =   testVar0FileIO progName result10A "resultB10" textstate2Text
test_11_A_B_textstate_text_11 =   testVar0FileIO progName result11A "resultB11" textstate2Text
test_12_A_B_textstate_text_12 =   testVar0FileIO progName result12A "resultB12" textstate2Text





--litTestDir1 = makeAbsDir "/home/frank/additionalSpace/DataBig/LitTest"
--sourceTest = TextSource {server = serverBrest, sourceDir = litTestDir1}
--destinationTest = DGoutDir litTestDir1
fill_ :: FilePath -> FilePath -> TextDescriptor

fill_ f1 f2 = fillTextState3a dirsTest serverBrest f1 f2 False  -- 3a not including text
--fill_ f1 f2 = fillTextState3a dirsTest serverLocalhost f1 f2 False  -- 3a not including text
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

--writeTextstate :: TextDescriptor -> ErrIO TextDescriptor
writeTextstate textstate = return (showT textstate)

test_1_A = testVar0FileIO progName result1A "resultA1" writeTextstate
test_2_A = testVar0FileIO progName result2A "resultA2" writeTextstate
test_3_A = testVar0FileIO progName result3A "resultA3" writeTextstate
test_4_A = testVar0FileIO progName result4A "resultA4" writeTextstate
test_5_A = testVar0FileIO progName result5A "resultA5" writeTextstate
test_6_A = testVar0FileIO progName result6A "resultA6" writeTextstate
test_7_A = testVar0FileIO progName result7A "resultA7" writeTextstate
test_8_A = testVar0FileIO progName result8A "resultA8" writeTextstate
test_9_A = testVar0FileIO progName result9A "resultA9" writeTextstate
test_10_A = testVar0FileIO progName result10A "resultA10" writeTextstate
test_11_A = testVar0FileIO progName result11A "resultA11" writeTextstate
test_12_A = testVar0FileIO progName result12A "resultA12" writeTextstate
