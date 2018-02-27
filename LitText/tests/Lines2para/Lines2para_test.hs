-----------------------------------------------------------------------------
--
-- Module      :  Parser . MarkupLines
-- Copyright   :  andrew u frank -
--
-- |  grouping the lines to paragraphs  - completes the parsing
-- TextZeilen is reading in , TZ is a conversion of TextZeilen (no IO)
-- works only on text lines
-- unparse the internal TZ representation and produce a tile to compare with the
--original txt file
-- does not show the page numbers ???
-- seitenzahlen must be numbers (not alpha) - is used to parse!
-- .ende is necessary to distribute page numbers!
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
--{-# OPTIONS_GHC -w #-}

module Lines2para.Lines2para_test  where

--import Lines2para.Lines2ignore
--import  Lines2para.HandleLayout -- TZ

--import           Data.List.Split
import           Uniform.Error
import           Uniform.Strings     hiding ((<|>), (</>))
import Uniform.FileIO
-- TODO string s
import Data.List (nub)
import           Test.Framework
import Uniform.TestHarness
import Parser.TextDescriptor -- (ParaNum (..), unparaNum)
import Lines2para.Lines2para


----test_0BA_BAC = testFile2File "resultBA0" "resultBAC0" paragraphs2TZpara
test_1C_CA = testFile2File "resultC1" "resultCA1" paragraphsTZ2TZ2
test_2C_CA = testFile2File "resultC2" "resultCA2" paragraphsTZ2TZ2
test_3C_CA = testFile2File "resultC3" "resultCA3" paragraphsTZ2TZ2
test_4C_CA = testFile2File "resultC4" "resultCA4" paragraphsTZ2TZ2
test_5C_CA = testFile2File "resultC5" "resultCA5" paragraphsTZ2TZ2
test_6C_CA = testFile2File "resultC6" "resultCA6" paragraphsTZ2TZ2
test_8C_CA = testFile2File "resultC8" "resultCA8" paragraphsTZ2TZ2
test_9C_CA = testFile2File "resultC9" "resultCA9" paragraphsTZ2TZ2
test_10C_CA = testFile2File "resultC10" "resultCA10" paragraphsTZ2TZ2
test_11C_CA = testFile2File "resultC11" "resultCA11" paragraphsTZ2TZ2
test_12C_CA = testFile2File "resultC12" "resultCA12" paragraphsTZ2TZ2

--test_8B_CA = testFile2File "resultBA8" "resultCA8" paragraphs2TZ





-- test text combinatioin zeilenText

--test_zeilenText = do
--    let res = map zeilenText t11
--    assertEqual t1_res res
--
--t1_res =
--    ["'Fury said to a mouse, That he met in the house. ",
--     "CHAPTER IV. The Rabbit Sends in a Little Bill",
--     "It was the White Rabbit, trotting slowly back again, and looking anxiously about as it went, as if it had lost something . "]


--t11 :: [TZ2]
--t11 =
--    [TZ2para{tz2loc = TextLoc{tlpage = Just "", tlline = 49},
--                 tz2tzs =
--                   [TZtext1{tzt1 = Kurz0, tzloc1 = TextLoc{tlpage = Just "", tlline = 50},
--                           tztext1 = TextWithMarks{twm = "'Fury said to a", twmMarks = []}
----                           tzlang1 = English
--                           },
--                    TZtext1{tzt1 = Kurz0, tzloc1 = TextLoc{tlpage = Just "", tlline = 51},
--                           tztext1 = TextWithMarks{twm = "mouse, That he", twmMarks = []}
----                           tzlang1 = English
--                           },
--                    TZtext1{tzt1 = Kurz0, tzloc1 = TextLoc{tlpage = Just "", tlline = 52},
--                           tztext1 = TextWithMarks{twm = "met in the", twmMarks = []}
----                           tzlang1 = English
--                           },
--                    TZtext1{tzt1 = Kurz0, tzloc1 = TextLoc{tlpage = Just "", tlline = 53},
--                           tztext1 = TextWithMarks{twm = "house.", twmMarks = []}
----                           tzlang1 = English
--                           }],
--                 tz2lang = English, tz2para = ParaNum 9, tz2inPart = ParaNum 4},
--        TZ2markup{tz2loc = TextLoc{tlpage = Just "", tlline = 55},
--                   tz2text =
--                     TextWithMarks{twm =
--                                     "CHAPTER IV. The Rabbit Sends in a Little Bill",
--                                   twmMarks = []},
--                   tz2tok = BuchHL1, tz2lang = English, tz2para = ParaNum 10,
--                   tz2inPart = ParaNum 1},
--         TZ2para{tz2loc = TextLoc{tlpage = Just "", tlline = 57},
--                 tz2tzs =
--                   [TZtext1{tzt1 = Text0, tzloc1 = TextLoc{tlpage = Just "", tlline = 57},
--                           tztext1 =
--                             TextWithMarks{twm =
--                                             "It was the White Rabbit, trotting slowly back again, and looking",
--                                           twmMarks = []}
----                           tzlang1 = English
--                           },
--                    TZtext1{tzt1 = Text0, tzloc1 = TextLoc{tlpage = Just "", tlline = 58},
--                           tztext1 =
--                             TextWithMarks{twm =
--                                             "anxiously about as it went, as if it had lost something .",
--                                           twmMarks = []}
----                           tzlang1 = English
--                            }],
--                 tz2lang = English, tz2para = ParaNum 11, tz2inPart = ParaNum 10}]


