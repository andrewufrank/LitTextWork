 -----------------------------------------------------------------------------
--
-- Module      :  Processor ProcessAll
-- Copyright   :  andrew u frank -
--
-- | finds all markup files and process them to store in triple store
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}


module Processor.ProcessAll
    (module Processor.ProcessAll
    , module Parser.Foundation
    , module Processor.Main2sub
    ) where

import           Test.Framework

import Parser.Foundation hiding ((<>) , (</>), (<.>))
import Producer.Servers
import Processor.Main2sub
import Lines2para.Lines2ignore (LanguageCode(..)) -- hiding ((<>) , (</>), (<.>))

-- import CoreNLP.Snippets2nt as Snippets2nt (nlp_serverLoc, host_serverLoc)

import qualified Pipes as Pipe
import qualified Pipes.Prelude as Pipe
import Pipes ((>->), (~>))
-- todo fileio - export for pipes


import Uniform.Error
import Uniform.FileIO
import Uniform.FileStatus
--import Uniform.Strings hiding ((</>),(<|>))
import Data.RDF.FileTypes (ntFileTriples)

processAll :: Bool ->  TextSource -> DestGenerality -> Path ar File  -> ErrIO ()
-- | get all markup files in the partially filled TextState2
-- the output goes to the second, not the triples
-- debug flag is not yet used

processAll debug ts dg  file  =  do
  let path = sourceDir ts
  resFile <- makeAbsolute file
  bracketErrIO (openFile2handle resFile WriteMode)
        (closeFile2)
        (\hand -> do
              Pipe.runEffect $
                getRecursiveContents path
                >-> Pipe.filter isMarkup --
                >-> Pipe.mapM (fmap t2s . processOneMarkup debug ts dg)
            --    >-> P.stdoutLn
                >-> Pipe.toHandle hand
        )

isMarkup :: Path Abs File -> Bool
isMarkup  = hasExtension (Extension "markup")
-- todo include in typedfiles - hasType ...

--debugNLP = False
--litDebugOnly = False

processOneMarkup :: Bool ->  TextSource -> DestGenerality -> Path Abs File
            -> ErrIO Text
-- process one markup file, if the nt file does not exist
processOneMarkup debug  ts dg lfp = do
        let textstate2 = fillTextState ts dg lfp
        putIOwords ["\nprocessOneMarkup", showT textstate2]
        ntExist <- exist6 (textfilename textstate2) ntFileTriples
        if not ntExist
            then do
                putIOwords  ["\nprocessMarkup - process"
                        , showT $ textfilename textstate2, "\n"]
                mainLitAndNLPproduction False False textstate2
                -- first bool is debug output
                -- second stops processing after lit (no nlp calls)
                putIOwords  ["\nprocessMarkup - processed"
                        , showT $ textfilename textstate2, "\n"]
                return (showT textstate2)
            else do
                putIOwords  ["\nprocessMarkup - nt file exist already"
                        , showT $ textfilename textstate2, "\n"]
                return . unlinesT $ ["\nprocessMarkup - nt file exist already"
                        , showT $ textfilename textstate2, "\n"]

    `catchError` \e -> do
            putIOwords  ["\nprocessMarkup - error return for "
                    , showT lfp, "\n"
                    , "error", e
                    , "not raised further to continue processing all"
                    ]
            return ""


fillTextState :: TextSource -> DestGenerality -> Path Abs File -> TextState2
fillTextState ts dg fp = fillTextState2 ts dg author buch
    where
        author = getImmediateParentDir fp
        buch = getNakedFileName fp

    ----------------- tests
--origDirForTest = "/home/frank/additionalSpace/DataBig/LitTest" :: FilePath
-- /home/frank/additionalSpace/DataBig/LitTest/carol
--shortTestDir = "/home/frank/additionalSpace/DataBig/LitTestShort"
shortTestDir2 = makeAbsDir "/home/frank/additionalSpace/DataBig/LitTestShort"

litTestDir1 = makeAbsDir "/home/frank/additionalSpace/DataBig/LitTest"
sourceTest4 = TextSource {server = serverBrest, sourceDir = litTestDir1}
generalityTest4 = DGoutDir litTestDir1

sourceShortTest = TextSource {server = serverBrest, sourceDir=shortTestDir2}
generalityShortTest = DGoutDir shortTestDir2

litOrigDir1 = makeAbsDir "/home/frank/additionalSpace/DataBig/LitOriginals"
sourceOrig4 = TextSource {server = serverBrest, sourceDir = litOrigDir1}
generalityOrig4 = DGoutDir litOrigDir1

test_0 = do
    res0 <- runErr $ do
        processAll False sourceShortTest generalityShortTest resfileN
    putIOwords ["test_0 - return:", showT res0]
    resN <- readFile5 resfileN
    res0 <- readFile5 resfile0
    assertEqual res0 resN

resfile0 = makeRelFile "resfile0"
resfileN = makeRelFile "resfileN"

--res0 = "LegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/minimal/minimal.text\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/minimal/minimal.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/panchatantra/The Panchatantra (Purnabhadra's Recension of 1199 CE).txt\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/panchatantra/pancha1_4000.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/panchatantra/pancha1.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/panchatantra/pancha1.txt\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/carol/test1.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/carol/test3.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/carol/test2.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/carol/minimal.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/carol/pg11.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/tawada/bad.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/tawada/Tawada-Fremde.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/tawada/Arufabetto.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/tawada/Auge.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/tawada/etueden.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/tawada/t3.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/tawada/t2.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/waterhouse/t3 (copy).markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/waterhouse/t1.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/waterhouse/t2b.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/waterhouse/t2utf_markup.txt\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/waterhouse/t2a.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/waterhouse/t3.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/waterhouse/t2.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/LaFontaine/7241test.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/LaFontaine/7241.txt\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/LaFontaine/7241.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/LaFontaine/7241xx.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/test/t1.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/test/test.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/test/pg12partial.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/test/in.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/test/t3.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/test/t4.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/test/t2.markup\"\n"
---- when only markup:
--resM = "LegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/minimal/minimal.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/panchatantra/pancha1_4000.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/panchatantra/pancha1.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/carol/test1.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/carol/test3.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/carol/test2.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/carol/minimal.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/carol/pg11.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/tawada/bad.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/tawada/Tawada-Fremde.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/tawada/Arufabetto.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/tawada/Auge.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/tawada/etueden.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/tawada/t3.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/tawada/t2.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/waterhouse/t3 (copy).markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/waterhouse/t1.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/waterhouse/t2b.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/waterhouse/t2a.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/waterhouse/t3.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/waterhouse/t2.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/LaFontaine/7241test.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/LaFontaine/7241.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/LaFontaine/7241xx.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/test/t1.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/test/test.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/test/pg12partial.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/test/in.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/test/t3.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/test/t4.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/test/t2.markup\"\n"
