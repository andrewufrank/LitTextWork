-----------------------------------------------------------------------------
--
-- Module      :  LitTypes . TextDescriptor
-- Copyright   :  andrew u frank -
--
-- | the definitions of the descrption of the text and related types (e.g. Language)
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
--{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}


module LitTypes.TextDescriptor_test where

import Uniform.HttpURI (URI, makeURI)
import           Test.Framework
import LitTypes.TextDescriptor
import Data.String
import Producer.Servers (serverBrest)

---- duplicates from Path.hs
--instance IsString (Path Abs File) where
--    fromString = f
--         where  f :: String -> Path Abs File
--                f s = read . show $ s
--instance IsString (Path Abs Dir) where
--    fromString = read
--instance IsString (Path Rel File) where
--    fromString = read
--instance IsString (Path Rel Dir) where
--    fromString = read

--deriving instance IsString (Path Abs File)

nd1 =  NTdescriptor {destNT =  "/home/frank/Scratch/NT/LitTest/test/t1"
            , gzipFlag = False}

td1 = TextDescriptor {
        sourceMarkup = "/home/frank/additionalSpace/DataBig/LitTest/test/t1"
        , nlpServer =  "http://nlp.gerastree.at"
        , authorDir = "test"
        , buchName = "t1"
        , includeText = False
        , txPosTagset = ""
        , ntdescriptor = nd1 }

test_read_textDescriptor = assertEqual td1 (read . show $ td1)  -- fail

td2 = "TextDescriptor {sourceMarkup = \"/home/frank/additionalSpace/DataBig/LitTest/test/t1\",\
\ nlpServer = \"http://nlp.gerastree.at\", authorDir = \"test\", buchName = \"t1\", \
\includeText = False, txPosTagset = \"\", ntdescriptor = NTdescriptor\
        \ {destNT = \"/home/frank/Scratch/NT/LitTest/test/t1\", gzipFlag = False}}"

test_r2 = assertEqual td1 (read td2) -- fail

td3 = TextDescriptor {sourceMarkup = "/home/frank/additionalSpace/DataBig/LitTest/test/t1",
    nlpServer = "http://nlp.gerastree.at", authorDir = "test", buchName = "t1",
    includeText = False, txPosTagset = ""
    , ntdescriptor = NTdescriptor
         {destNT = "/home/frank/Scratch/NT/LitTest/test/t1", gzipFlag = False}
         }

td4 = TextDescriptor {sourceMarkup = f1p,
    nlpServer = serverBrest, authorDir = "test", buchName = "t1",
    includeText = False, txPosTagset = ""
    , ntdescriptor = NTdescriptor
         {destNT = f1p, gzipFlag = False}
         }

td4s = show td4

test_td4s = do              -- ok
        putIOwords ["test_td4s - td4 is:", showT td4]
        putIOwords ["test_td4s - td4s is:", s2t td4s]
        putIOwords ["test_td4s - show td4 is:", showT td4]
        assertEqual td4  (read $ td4s)

td5 = TextDescriptor {sourceMarkup = "/home/frank/additionalSpace/DataBig/LitTest/test/t1",
    nlpServer = serverBrest, authorDir = "test", buchName = "t1",
    includeText = False, txPosTagset = ""
    , ntdescriptor = NTdescriptor
         {destNT = "/home/frank/additionalSpace/DataBig/LitTest/test/t1", gzipFlag = False}
         }

--td4 = read td2 :: String   -- no parse
--test_td4s = do
--        putIOwords ["test_td3s - td4 is:", s2t td4]
--        putIOwords ["test_td3s - show td3 is:", showT td3]
--        assertEqual td3  (read $ td4)

twoOut :: IO ()
twoOut = do
        putIOwords ["twoOut", "start"]
        putIOwords ["test_td4 - td4 is:", showT td4]
        putIOwords ["test_td5 - td5 is:", showT td5]
--        putIOwords ["test_td4s - td3s is:", s2t td3s]
--        putIOwords ["test_td4s - show td3 is:", showT td3]
        putIOwords ["twoOut end " ]


f1p = makeAbsFile "/home/frank/additionalSpace/DataBig/LitTest/test/t1"
f1 = "\"/home/frank/additionalSpace/DataBig/LitTest/test/t1\""
test_f1 = assertEqual f1p (read f1)


data Xt = Xt { p :: Path Abs File
                , q :: Text
                } deriving (Show, Read, Eq)

f3 = "/somedir/more/afile.ext"  :: FilePath
x1f = makeAbsFile f3 :: Path Abs File
xt = Xt x1f "f3"
--xt3 = Xt "/somedir/more/afile.ext" "f3"
xts = show xt

test_xt1 = do
        putIOwords ["xt1 - xts is:", s2t xts]
        putIOwords ["xt1 - show xt is:", showT xt]
        assertEqual xt  (read $ xts)

xt2 = Xt {p = "/somedir/more/afile.ext", q = "f3"}

--
--fillDestination :: DestGenerality -> FilePath -> FilePath -> Bool -> DestDescriptor
---- | build the description of the destination
---- either a file or a uri with grah
---- later add handle with switch - true for file append output
---- false for use a handle
--fillDestination  (DGoutDir dir) author buch True = OutFile (dir </> (author </> buch))
--fillDestination  t _ _ _ = errorT ["Foundation - fillDestination not defined for ", showT t]
--
--
--test_fillTextState10 = assertEqual res10 res
--    where
--        res = fillTextState2 sourceE1 generalityE1 "may" "test"
--res10 =  TextState2 {source = TextSource
--                        {server = makeURI "http://nlp.gerastree.at",
--                        sourceDir = makeAbsDir "/home/frank/additionalSpace/DataBig/LitTest/"},
--                authorDir ="may",
--                buchname = "test",
--            textfilename = makeAbsFile "/home/frank/additionalSpace/DataBig/LitTest/may/test",
--            tripleOutDesc = OutFile
--                {ddFile = makeAbsFile "/home/frank/additionalSpace/DataBig/LitTest/may/test"}}

