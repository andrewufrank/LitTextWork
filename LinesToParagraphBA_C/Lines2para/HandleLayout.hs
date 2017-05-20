-----------------------------------------------------------------------------
--
-- Module      :  Parser . MarkupLines
-- Copyright   :  andrew u frank -
--
-- |  grouping the lines to paragraphs  - completes the parsing
-- TextZeilen is reading in , TZ is a conversion of TextZeilen (no IO)
-- works only on text lines
-- unpare the internal TZ representation and produce a tile to compare with the
--original txt file
-- does not show the page numbers
-- seitenzahlen must be numbers (not alpha) - is used to parse!
-- .ende is necessary to distribute page numbers!
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
--{-# OPTIONS_GHC -fno-warn-missing-methods #-}
--{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
--{-# OPTIONS_GHC -w #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Lines2para.HandleLayout
    (module Lines2para.HandleLayout
    , module BuchCode.MarkupText
    , module Uniform.Error
    ) where
-- (htf_thisModulesTests
--    , paragraphs2TZlayout
--    , distributePageNrs , etts2tzs
--    , TZ (..), TextLoc (..)
--        )  where

import Test.Framework
--import Parser.Foundation
import BuchCode.MarkupText
--    (Zeilen (..), TextZeilen (..), TextType (..)
--            , TextWithMarks (..))
--import BuchCode.BuchToken
--    (LanguageCode (..), BuchToken (..), markerPure)
import           Data.List.Split
-- todo strings
import           Uniform.Error
--import           Uniform.Strings     hiding ((<|>), (</>))
import Uniform.Zero
-- todo include zero  in error and strings
--import Uniform.FileIO
-- TODO string s
--import Data.List (nub)
--import           Text.Printf         (printf)


data TextLoc = TextLoc {tlpage :: Text, tlline :: Int} deriving (Show, Eq)
-- ^ the place of a line in the full text
-- for simplification, all counts are from the start of the text
-- not relative to the page or paragraph (can be computed, if desired)
-- page number (tlline) is text, to deal with III etc.
    -- removed paraid

instance Zeros TextLoc where zero = TextLoc zero zero


-- the format accumulation all detail info to build the triples.
-- only tzpara and tzmarkup in final result
data TZ =
         TZtext {tzt:: TextType, tzloc :: TextLoc
                    , tztext:: TextWithMarks
                    , tzlang :: LanguageCode }
--        | TZpara  {tzloc :: TextLoc, tztzs :: [TZ], tzlang :: LanguageCode
--                , tlpara :: ParaNum
--                , tzInPart :: ParaNum}
        | TZmarkup  {tzloc :: TextLoc, tztext:: TextWithMarks
                        , tztok :: BuchToken, tzlang :: LanguageCode
--                        , tlpara :: ParaNum
--                        , tzInPart :: ParaNum
                        }
        | TZleer  {tzloc :: TextLoc}
        | TZneueSeite  {tzloc :: TextLoc}
        | TZignore {tzloc :: TextLoc, tztext:: TextWithMarks}
            deriving (Show, Eq )

instance Zeros TZ where zero = TZleer zero


ett2tz :: TextZeilen -> TZ
-- convert the textzeilen to tz without filling location
-- some markup is converted to lit text
ett2tz (TextZeile ty t) = TZtext {tzt=ty, tztext = t, tzloc = zero, tzlang=zero }
--ett2tz (ZahlZeile t) = TZzahl {tztext = t, tzloc = zero, tzlang=zero}
ett2tz (MarkupZeile BuchIgnore t) = TZignore {tztext = t,  tzloc = zero}
ett2tz (MarkupZeile BuchGedicht t) =
    TZtext {tzt=Kurz0, tztext = t,  tzloc = zero, tzlang=zero}
ett2tz (MarkupZeile BuchEnde t) = TZleer {tzloc = zero}
-- will be filtered out
ett2tz (MarkupZeile tok t) = TZmarkup {tztext = t, tztok = tok, tzloc = zero
        , tzlang=zero
--        , tlpara = zero , tzInPart = zero
             }
ett2tz LeerZeile = TZleer {tzloc = zero}
ett2tz (NeueSeite) = TZneueSeite {tzloc = zero}
ett2tz x = errorT ["ett2tz not prepared for pattern", showT x]

etts2tzs :: [TextZeilen] -> [TZ]
-- gives line numbers
etts2tzs = zipWith line2tz [1..] . map ett2tz

line2tz :: Int -> TZ -> TZ
-- ^ fill a TZ with the linenumber
line2tz i tz = tz {tzloc = (tzloc tz) {tlline =  i} }


unparseTZs :: [TZ] -> Text
-- produce a text which can be written to a file and compared with the original
unparseTZs = concat' . map renderZeile

paragraphs2TZlayout :: [TextZeilen] -> [TZ]  -- test BA -> C
-- ^ produce the paragraphs with the seitenzahlen in each line
paragraphs2TZlayout =
    distributePageNrs
    . etts2tzs
    -- test BA -> BAA ... BAG -> C

-------------------------PAGES

distributePageNrs :: [TZ] -> [TZ]
-- mark the zeilen with the page number
-- no pagenumber left
-- page numbers are asumed at he bottom of the page!
distributePageNrs  =  checkSeitenzahl . concat .   markSublist . pages
    where
        pages :: [TZ] -> [[TZ]]
        pages tz = (split .  keepDelimsR . whenElt) isSeitenzahl tz
            -- (TZzahl zero zero : tz)
        -- to start with a page 0  -- break after page number
        -- appends the page to the sublist
        pageNrOfSublist :: [TZ] -> Maybe Text
        pageNrOfSublist tzs = if isSeitenzahl lasttz then Just . zeilenText  $ lasttz -- isSeitenzahl
                                            else Nothing
            where
                lasttz = last tzs

        markSublist :: [[TZ]] -> [[TZ]]
        markSublist []  = []
        markSublist sls =  map markSublist2 (init sls) ++ [last sls]
--            [markSublist2 $ head sls ] ++ (map markSublist2 . tail $ sls)
--the last sublist contains just the end mark
        markSublist2 :: [TZ] -> [TZ]
        markSublist2 [] = []
        markSublist2 sl = markTZsWithPage (fromJustNote "distributePageNrs" $ pageNrOfSublist sl)
                                    (init sl)

markTZsWithPage :: Text -> [TZ] -> [TZ]
-- put the page number into the list
markTZsWithPage i  = map  (\tz -> tz {tzloc = (tzloc tz) {tlpage = i} } )

checkSeitenzahl [] = []
checkSeitenzahl (t:ts) = if isSeitenzahl t then errorT ["checkSeitenzahl found one ", showT t]
                                else t : checkSeitenzahl ts

instance Zeilen TZ where
    isLeerzeile TZleer  {} = True
    isLeerzeile _          = False

    isSeitenzahl (TZtext {tzt = Zahl0}) = True
    isSeitenzahl _          = False

    isTextZeile (TZtext {tzt = tyt}) = tyt `elem` [Text0, Para0, Kurz0]
    isTextZeile _         = False

    isMarkupZeile TZmarkup {} = True
    isMarkupZeile _           = False

    isKurzeZeile (TZtext {tzt=Kurz0}) = True
    isKurzeZeile _         = False

    isNeueSeite TZneueSeite {} = True
    isNeueSeite _ = False

    isMarkupX code TZmarkup{tztok=c} =  code == c
    isMarkupX code _                 = False

    zeilenText TZleer {} = ""
    zeilenText (TZtext {tztext=tx}) =  twm tx
    zeilenText _ = ""

--    renderZeile  tz =  case tz of
--        TZleer {} -> ""
--        TZzahl {} -> errorT ["renderZeile seitenzahl", "should not occur", showT tz]
--        TZmarkup {tztok=tok} -> unwords' ["." <> markerPure (tztok tz), tztext tz] <>
----                        if tok == BuchHL1 then
--                                " - para " <> (unparaID . tlpara . tzloc $ tz) <>
--                                " in " <> (unparaID . tzInPart $ tz) <>
----                                " tok "
----                                (markerPure . tztok $ tz) <>  "  text"
----                                (tztext $ tz) <>
--                                "\n"
----                                            else "\n"
--        -- leerzeile nach markup
--        -- TZkurz {} ->  tztext tz <> "\n"
--        TZtext {} ->  tztext tz <> "\n"
--        TZpara {} -> "\n" <> (unparaID . tlpara . tzloc $ tz)
--                <> " in " <> (unparaID . tzInPart $ tz)
--                <> " on page " <> (tlpage . tzloc $ tz)
--                <> "\n" <> (concat' . map renderZeile  . tztzs $ tz) <> "\n"


--filterZeilen :: [TZ] -> [TZ]
---- ^ remove some lines - here the neueSeite, where i have no idea what to do with
--filterZeilen = id -- filter (not.isNeueSeite)


-- test the first (expected ok) part of the chain
test_0_BA_BAC =do
        putIOwords ["test_0_BA_BAC", "from result0BA_tz_markupResult1 to result0BAC"]
        assertEqual result0BAC (paragraphs2TZlayout result0BA)
test_1_BA_BAC =do
        putIOwords ["test_1_BA_BAC", "from result1BA_tz_markupResult1 to result1BAC"]
        assertEqual result1BAC (paragraphs2TZlayout result1BA)
test_2_BA_BAC =do
        putIOwords ["test_2_BA_BAC", "from result2BA_tz_markupResult1 to result2BAC"]
        assertEqual result2BAC (paragraphs2TZlayout result2BA)
test_5_BA_BAC =do
        putIOwords ["test_5_BA_BAC", "from result5BA_tz_markupResult1 to result5BAC"]
        assertEqual result5BAC (paragraphs2TZlayout result5BA)
test_6_BA_BAC =do
        putIOwords ["test_6_BA_BAC", "from result6BA_tz_markupResult1 to result6BAC"]
        assertEqual result6BAC (paragraphs2TZlayout result6BA)
----------- test results


#include "LayoutResults.res"

