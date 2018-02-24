-----------------------------------------------------------------------------
--
-- Module      :  Parser . TextDescriptor
-- Copyright   :  andrew u frank -
--
-- | the definitions of the descrption of the text and related types (e.g. Language)
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parser.TextDescriptor_test where

-- import           Data.RDF.Extension
import           Uniform.FileIO  -- (Path (..), Abs, Dir, File)
import           Uniform.Strings hiding ((</>), (<.>))   -- hiding ((<|>))
import System.IO (Handle)  -- todo include in FileIO exports
import Uniform.HttpURI (URI)
import Producer.Servers  (serverBrest)  -- for test
import Data.RDF.Extension (LanguageCode (..), RDFtypes(..), RDFproperties (..))
import Data.RDF.FileTypes
import           Test.Framework
import BuchCode.BuchToken hiding ((</>), (<.>))
import Parser.LanguageTypedText
import Process.UtilsParseArgs (LitTextFlags (..) )
import Parser.TextDescriptor


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
test_fillTextState10 = assertEqual res10 res
    where
        res = fillTextState2 sourceE1 generalityE1 "may" "test"
res10 =  TextState2 {source = TextSource
                        {server = makeURI "http://nlp.gerastree.at",
                        sourceDir = makeAbsDir "/home/frank/additionalSpace/DataBig/LitTest/"},
                authorDir ="may",
                buchname = "test",
            textfilename = makeAbsFile "/home/frank/additionalSpace/DataBig/LitTest/may/test",
            tripleOutDesc = OutFile
                {ddFile = makeAbsFile "/home/frank/additionalSpace/DataBig/LitTest/may/test"}}


