-----------------------------------------------------------------------------
--
-- Module      :  Parser . deal with ignored lines
-- Copyright   :  andrew u frank -
--
-- |  deal wit ignored lines and language
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}

module Lines2para.Lines2ignore
    (module Lines2para.Lines2ignore
    , module Lines2para.HandleLayout

    ) where


--import BuchCode.MarkupText
--import BuchCode.BuchToken
import Lines2para.HandleLayout

import           Data.List.Split
-- TODO string s
--import Data.List (nub)
import           Test.Framework
import Uniform.TestHarness


paragraphs2TZsimple :: [TZ] -> [TZ]  -- test BA -> C
-- ^ produce the text files (ignores removed, language marked)
-- but not paragraphs
-- page number and line numbers are in layout
paragraphs2TZsimple =
    map fixTextAllCaps .
    distributeLanguage .
    distributeIgnore
    -- test BA -> BAD

fixTextAllCaps ::  TZ  ->  TZ
fixTextAllCaps TZtext {tzt=AllCaps0, ..} = TZmarkup{tzloc = tzloc
            , tztext=tztext, tztok= BuchHL2, tzlang=tzlang}
            -- can later be dealt with similar to language
            -- with a switch in the markup file
fixTextAllCaps tz = tz


---- test the first (expected ok) part of the chain
--test_1_BA_BAD =do
--        putIOwords ["test_1_BA_BAD", "from result1BA_tz_markupResult1 to result1BAD"]
--        assertEqual result1BAD
--            (paragraphs2TZsimple  result1BAC)
--test_2_BA_BAD =do
--        putIOwords ["test_2_BA_BAD", "from result2BA_tz_markupResult1 to result2BAD"]
--        assertEqual result2BAD
--                (paragraphs2TZsimple result2BAC)
--test_5_BA_BAD =do
--        putIOwords ["test_5_BA_BAD", "from result5BA_tz_markupResult1 to result5BAD"]
--        assertEqual result5BAD
--                (paragraphs2TZsimple result5BAC)
--test_6_BA_BAD =do
--        putIOwords ["test_6_BA_BAD", "from result6BA_tz_markupResult1 to result6BAD"]
--        assertEqual result6BAD
--                (paragraphs2TZsimple result6BAC)

--test_0BAC_BAD = testFile2File "resultBAC0" "resultBAD0" paragraphs2TZsimple
test_1BAC_BAD = testFile2File "resultBAC1" "resultBAD1" paragraphs2TZsimple
test_2BAC_BAD = testFile2File "resultBAC2" "resultBAD2" paragraphs2TZsimple
test_3BAC_BAD = testFile2File "resultBAC3" "resultBAD3" paragraphs2TZsimple
test_4BAC_BAD = testFile2File "resultBAC4" "resultBAD4" paragraphs2TZsimple
test_5BAC_BAD = testFile2File "resultBAC5" "resultBAD5" paragraphs2TZsimple
test_6BAC_BAD = testFile2File "resultBAC6" "resultBAD6" paragraphs2TZsimple

------------LANGUAGE

distributeLanguage :: [TZ] -> [TZ]
-- mark the zeilen with the language
-- removes the language markup lines
distributeLanguage tz0 = concat . markSublistLanguage . pages $ tz0
    where
        pages :: [TZ] -> [[TZ]]
        pages  = (split .  keepDelimsL . whenElt) isLanguageCode
--      prepends
        isLanguageCode TZmarkup{tztok=BuchSprache} = True
        isLanguageCode _                 = False

        getLangCode _ tz3@TZmarkup{tztext=tx}  =
                    readLanguageCode "distributeLanguage" .twm $ tx
        getLangCode l tz3 = errorT ["distributeLanguage - languageCode 2"
                , showT tz3, "\n"
                , unlines' . map showT $ l
                , "fulllist is \n", unlines' . map showT $ tz0]

        markSublistLanguage :: [[TZ]] -> [[TZ]]
        markSublistLanguage []       = []
        markSublistLanguage (s1:sl1) = s1 : map markSublistLanguage2 sl1
        -- s1 is the partial start sublist with no language code

        markSublistLanguage2 :: [TZ] -> [TZ]
        markSublistLanguage2 [] = []
        markSublistLanguage2 sl2 = markTZsWithLanguage
                (getLangCode sl2 . headNote "distributeLanguage" $ sl2) (tail sl2)

readLanguageCode :: Text -> Text -> LanguageCode
-- ^ read the code for the language German, Deutsch, Englisch
--readLanguageCode  = readNoteT
-- todo move to parser
readLanguageCode _ "Deutsch" = German
readLanguageCode msg l  = readNoteT msg l

markTZsWithLanguage :: LanguageCode -> [TZ] -> [TZ]
-- put the page number into the list
markTZsWithLanguage lg = map  (markoneLanguage lg)
    where
        markoneLanguage lg tz@TZtext {} = tz {tzlang = lg }
        markoneLanguage lg tz@TZmarkup {} = tz {tzlang = lg }
        markoneLanguage lg tz = tz

------- distribute ignore to  ignore end TODO
distributeIgnore :: [TZ] -> [TZ]
-- mark the zeilen with the Ignore
distributeIgnore  = concat . markSublistIgnore . pages
    where
        pages :: [TZ] -> [[TZ]]
        pages = (split .  keepDelimsL . whenElt) isIgnoreCode
--      prepends
        isIgnoreCode TZmarkup{tztok=BuchIgnoreTo} = True
        isIgnoreCode TZmarkup{tztok=BuchIgnoreEnd} = True
        isIgnoreCode _            = False

        -- getLangCode _ tz3@TZmarkup{}  = readIgnoreCode "distributeIgnore" . tztext $ tz3
        -- getLangCode l tz3 = errorT ["distributeIgnore - IgnoreCode 2", showT tz3, "\n"
        --         , unlines' . map showT $ l
        --         , "fulllist is \n", unlines' . map showT $ tz0]

        markSublistIgnore :: [[TZ]] -> [[TZ]]
        markSublistIgnore []       = []
        markSublistIgnore (s1:sl1) = s1 : map markSublistIgnore2 sl1
        -- s1 is the partial start sublist with no Ignore code

        markSublistIgnore2 :: [TZ] -> [TZ]
        markSublistIgnore2 [] = []
        markSublistIgnore2 (s2: sl2) = case tztok s2 of
        -- a list of sublist with a ignore at start
                    BuchIgnoreTo  -> markTZsWithIgnore sl2  -- this is a sublist to ignore
                    BuchIgnoreEnd -> sl2  -- this is to keep
                    _             -> errorT ["markSublistIgnore2", showT s2]

-- readIgnoreCode :: Text -> Text -> IgnoreCode
-- readIgnoreCode msg t = readNoteT msg t

markTZsWithIgnore ::  [TZ] -> [TZ]
-- this should convert all lines, independent to ignorezeile
--markTZsWithIgnore  = map  (\tz1 -> TZignore {tzloc = tzloc tz1, tztext = tztext tz1, tzlang = tzlang tz1 } )
markTZsWithIgnore  = map  markoneWithIgnore
    where
        markoneWithIgnore  tz1@TZtext {} =
                TZignore {tzloc = tzloc tz1, tztext = tztext tz1 }
--        markoneWithIgnore  tz1@TZmarkup {} =
--                TZignore {tzloc = tzloc tz1, tztext = tztext tz1 }
-- keep the markup codes in the ignore blocks, simplifies markup
-- possibly automatic from guttenberg texts
        markoneWithIgnore  tz1 = tz1

-- #include "Lines2ignoreTestResults.res"


