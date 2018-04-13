-----------------------------------------------------------------------------
--
-- Module      :  Parser . MarkupLines
-- Copyright   :  andrew u frank -
--
-- | a parser for a line oriented file
-- especially with lines starting with a markup char ('.')
-- following the example in Real World Haskell

-- this produces a list of encoded lines TextZeile but not yet blocks.
-- ignore is parsed as a markup
-- to use automatic hl2 detection - replace in gutenberg ".--" -- not required anymore
-- does not read language
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings     #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module LitText.Lines.MarkupText_test  where


--import           BuchCode.BuchToken hiding (try, (<|>), (</>))
import           Data.Char
import Data.Maybe  -- todo string - algebras?
import           Uniform.FileIO   hiding (try, (<|>))
import           Test.Framework
import LitText.Parser.Parser
import Uniform.Test.TestHarness hiding (try)
import LitText.Foundation
--    hiding (try, (<|>)) -- from Foundation

import LitText.Lines
import LitText.Lines.Lines
import LitText.Foundation

progName = "tests"
instance ShowTestHarness [TextZeile]

test_read1 = assertEqual "some text" (read . show $ "some text")

parseMarkupTest :: Text -> [TextZeile]
parseMarkupTest =  parseMarkup

--test_0B_BA = assertEqual result0BA (parseMarkup result0B)
-- local test

-- result1A, .. result6A is exported form ReadMarkupAB.

--test_0B_BA = test1File progName "resultB0" "resultBA0" parseMarkupTest
----test_1B_BA :: IO ()
test_1B_BA = test1File progName "resultB1" "resultBA1" parseMarkupTest
test_2B_BA = test1File progName "resultB2" "resultBA2" parseMarkupTest
test_3B_BA = test1File progName "resultB3" "resultBA3" parseMarkupTest
test_4B_BA = test1File progName "resultB4" "resultBA4" parseMarkupTest
test_5B_BA = test1File progName "resultB5" "resultBA5" parseMarkupTest
test_6B_BA = test1File progName "resultB6" "resultBA6" parseMarkupTest
test_8B_BA = test1File progName "resultB8" "resultBA8" parseMarkupTest  -- aesop
test_9B_BA = test1File progName "resultB9" "resultBA9" parseMarkupTest  -- tawada
test_10B_BA = test1File progName "resultB10" "resultBA10" parseMarkupTest  -- boccaccio
test_11B_BA = test1File progName "resultB11" "resultBA11" parseMarkupTest  -- italian testing
test_12B_BA = test1File progName "resultB12" "resultBA12" parseMarkupTest  -- italian testing




--------------------
result0B = unlines'  ["wort1;langeswort2"
            ,"55"
            ,".sprache German"
            ,""
            ,"1960 is a good"
            ,"eine kurze [vielleicht wichtige] zeile"
            ,"66"
            , "als\n\f\n[54/0002]\nda\223 man ihm erstens,"
            , "als\r\n\f\r\n[54/0002]\r\nda\223 man[2] ihm [1] mit Fussnoten,"
            ,""
            ,".titel TIT [3]"
            ,"zweite,[4] [5][6] kurze zeile[3]"
            ,"II.--THE COUNCIL HELD BY THE RATS [4]"
            ,"   Old Rodilard,[5] a certain cat,"
            ,"II - ALL CAPS TEST"
            , "[44]"  -- seitenzahl
            , "[1] eine Fussnote"
            ,"77"] ::  Text

result0BA =

    [TextZeile{ttt = Kurz0,
               ttx = TextWithMarks{twm = "wort1;langeswort2", twmMarks = []}},
     TextZeile{ttt = Zahl0,
               ttx = TextWithMarks{twm = "55", twmMarks = []}},
     MarkupZeile{ttok = BuchSprache,
                 ttx = TextWithMarks{twm = "German", twmMarks = []}},
     LeerZeile,
     TextZeile{ttt = Kurz0,
               ttx = TextWithMarks{twm = "1960 is a good", twmMarks = []}},
     TextZeile{ttt = Text0,
               ttx =
                 TextWithMarks{twm = "eine kurze [vielleicht wichtige] zeile",
                               twmMarks = []}},
     TextZeile{ttt = Zahl0,
               ttx = TextWithMarks{twm = "66", twmMarks = []}},
     TextZeile{ttt = Kurz0,
               ttx = TextWithMarks{twm = "als", twmMarks = []}},
     NeueSeite,
     TextZeile{ttt = Zahl0,
               ttx = TextWithMarks{twm = "[54/0002]", twmMarks = []}},
     TextZeile{ttt = Text0,
               ttx =
                 TextWithMarks{twm = "da\223 man ihm erstens,", twmMarks = []}},
     TextZeile{ttt = Kurz0,
               ttx = TextWithMarks{twm = "als", twmMarks = []}},
     NeueSeite,
     TextZeile{ttt = Zahl0,
               ttx = TextWithMarks{twm = "[54/0002]", twmMarks = []}},
     TextZeile{ttt = Text0,
               ttx =
                 TextWithMarks{twm = "da\223 man ihm  mit Fussnoten,",
                               twmMarks = [(7, "[2]"), (5, "[1]")]}},
     LeerZeile,
     MarkupZeile{ttok = BuchTitel,
                 ttx = TextWithMarks{twm = "TIT", twmMarks = [(5, "[3]")]}},
     TextZeile{ttt = Text0,
               ttx =
                 TextWithMarks{twm = "zweite,  kurze zeile",
                               twmMarks = [(7, "[4]"), (1, "[5]"), (0, "[6]"), (12, "[3]")]}},
     TextZeile{ttt = Text0,
               ttx =
                 TextWithMarks{twm = "II.--THE COUNCIL HELD BY THE RATS",
                               twmMarks = [(34, "[4]")]}},
     TextZeile{ttt = Text0,
               ttx =
                 TextWithMarks{twm = "Old Rodilard, a certain cat,",
                               twmMarks = [(16, "[5]")]}},
     TextZeile{ttt = Kurz0,
               ttx = TextWithMarks{twm = "II - ALL CAPS TEST", twmMarks = []}},
     TextZeile{ttt = Zahl0,
               ttx = TextWithMarks{twm = "[44]", twmMarks = []}},
     TextZeile{ttt = Fussnote0,
               ttx =
                 TextWithMarks{twm = "[1]eine Fussnote", twmMarks = [(0, "[1]")]}},
     TextZeile{ttt = Zahl0,
               ttx = TextWithMarks{twm = "77", twmMarks = []}}]
