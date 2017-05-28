-----------------------------------------------------------------------------
--
-- Module      :  Parser . MarkupLines  -- not used
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
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}

module xxLines2para.Lines2paraTests(  htf_thisModulesTests   -- for tests
--        , result4A, result2A, result3A
--    , result4A, result2A, result3A, result4A   -- fromReadMarkp, is textstate used widely
    , result1BA, result2BA, result3BA, result4BA
    , result1C, result2C, result3C, result4C
--        , result4C
)  where


import           Test.Framework
--import Parser.ReadMarkupAB
--import qualified BuchCode.MarkupText as ABA  -- (TextZeilen(..))
import  BuchCode.MarkupText
import BuchCode.BuchToken
import Lines2para.HandleLayout
import Lines2para.Lines2para
import           Data.List.Split
--import           Parser.Foundation   hiding ((</>)) -- gives TZ
import           Uniform.Error
import           Uniform.Strings     hiding ((<|>), (</>))
import Uniform.FileIO
import Data.List (nub)





test_1_BA_C = assertEqual result1C (paragraphs2TZ result1BA)
test_2_BA_C = assertEqual result2C (paragraphs2TZ result2BA)
test_3_BA_C = assertEqual result3C (paragraphs2TZ result3BA)
test_4_BA_C = assertEqual result4C (paragraphs2TZ result4BA)
test_5_BA_C = assertEqual result5C (paragraphs2TZ result5BA)



-- test the first (expected ok) part of the chain
test_1_BA_BAD =do
        putIOwords ["test_1_BA_BAD", "from result1BA_tz_markupResult1 to result1BAD"]
        assertEqual result1BAD
            (distributeIgnore . distributeLanguage . distributePageNrs . filterZeilen . etts2tzs
                $ result1BA)
test_2_BA_BAD =do
        putIOwords ["test_2_BA_BAD", "from result2BA_tz_markupResult1 to result2BAD"]
        assertEqual result2BAD
                (distributeIgnore . distributeLanguage . distributePageNrs . filterZeilen .etts2tzs
                    $ result2BA)

test_1_BAD_BAE = do
        putIOwords ["test_1_BAD_BAE", "from result1BAD to result1BAE"]
        assertEqual result1BAE (formParagraphs result1BAD)

test_1_BAE_C = do
        putIOwords ["test_1_BAE_C", "from result1BAE to result1C_tzResult1"]
        assertEqual result1C (distributeHeader . markParaNr $ result1BAE)


test_2_BAD_BAE = do
        putIOwords ["test_2_BAD_BAE", "from result2BAD to result2BAE"]
        assertEqual result2BAE (formParagraphs result2BAD)

--test_2_BAD_BAE = do
--        putIOwords ["test_2_BAD_BAE", "from result2BAD to result2BAE"]
--        assertEqual result2BAE (formParagraphs result2BAD)
--
--test_2_BAD_BAE = do
--        putIOwords ["test_2_BAD_BAE", "from result2BAD to result2BAE"]
--        assertEqual result2BAE (formParagraphs result2BAD)

test_2_BAE_C = do
        putIOwords ["test_2_BAE_C", "from result2BAE to result2C_tzResult1"]
        assertEqual result2C (distributeHeader . markParaNr
                    $ result2BAE)


--------------------------- T E S T S



-- these tests are here
-- use imported values
--test_1_BA_import_parseMarkup_1 = assertEqual result1BA ABA.result1BA
--test_2_BA_import_parseMarkup_2 = assertEqual result2BA ABA.result2BA
--test_3_BA_import_parseMarkup_3 = assertEqual result3BA ABA.result3BA
--test_4_BA_import_parseMarkup_4 = assertEqual result4BA ABA.result4BA


-- -- these tests are here
-- test_4_B_BA_parseMarkup_4 = assertEqual result4BA
--     (parseMarkup result4B)
-- test_2_B_BA_parseMarkup_2 = assertEqual result2BA
--     (parseMarkup result2B)
-- test_3_B_BA_parseMarkup_3 = assertEqual result3BA
--     (parseMarkup result3B)
-- test_4_B_BA_parseMarkup_3 = assertEqual result4BA
--     (parseMarkup result4B)


--

#include "Lines2paraTestResults.res"
