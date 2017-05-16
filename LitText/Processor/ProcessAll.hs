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
    (htf_thisModulesTests
    , processAll
    ) where

import           Test.Framework

import Parser.Foundation
import Main2sub
import CoreNLP.Snippets2nt as Snippets2nt (nlp_serverLoc)

import qualified Pipes as Pipe
import qualified Pipes.Prelude as Pipe
import Pipes ((>->), (~>))
-- todo fileio - export for pipes


import Uniform.Error
import Uniform.FileIO
import Uniform.FileStatus
import Uniform.Strings hiding ((</>),(<|>))



processAll :: TextState2 ->   Path ar File  -> ErrIO ()
-- | get all markup files in the partially filled TextState2
-- the result goes to the second
processAll textstate0  file  =  do
  let path = originalsDir textstate0
  resFile <- makeAbsolute file
  bracketErrIO (openFile2handle resFile WriteMode)
                (closeFile2)
                (\hand -> do
                      Pipe.runEffect $
                        getRecursiveContents path
                        >-> Pipe.filter isMarkup --
                        >-> Pipe.mapM (fmap t2s . processOneMarkup textstate0)
                    --    >-> P.stdoutLn
                        >-> Pipe.toHandle hand
                )

isMarkup :: Path Abs File -> Bool
isMarkup  = hasExtension (Extension "markup")
-- todo include in typedfiles - hasType ...

--filterMarkup :: [LegalPathname] -> [LegalPathname]
--filterMarkup   = filter (hasExtension "markup")

--debugNLP = False
litDebugOnly = False

processOneMarkup :: TextState2 -> Path Abs File -> ErrIO Text
processOneMarkup textstate0 lfp = do
    let textstate2 = fillTextState textstate0 lfp
    putIOwords ["processOneMarkup", showT textstate2]
    mainLitAndNLPproduction litDebugOnly textstate2
    return (showT textstate2)

fillTextState :: TextState2 -> Path Abs File -> TextState2
fillTextState textState0 fp = textState0 {  -- enthaelt endpotin, originalsDir
                              authorDir = getImmediateParentDir  $ fp
                              , buchname = getNakedFileName fp
                              , textfilename = fp
--                            , graph = filename2text fp a   -- the graph is the same as the author
                            }
    ----------------- tests
origDirForTest = "/home/frank/additionalSpace/DataBig/LitTest" :: FilePath
-- /home/frank/additionalSpace/DataBig/LitTest/carol


textstate0 = TextState2 {
--      endpoint = "http://127.0.0.1:3030/testDB/update"

        serverLoc = nlp_serverLoc  -- "http://nlp.gerastree.at"  -- "http://127.0.0.1"
        , originalsDir = makeAbsDir origDirForTest
        , authorDir = ""
        , buchname = ""
--        , graph = "automaticTest"
		, textfilename = makeAbsFile "/unusable"
        }

test_0 = do
    runErr $ do
        processAll textstate0 resfileN
    resN <- readFile5 resfileN
    res0 <- readFile5 resfile0
    assertEqual res0 resN

resfile0 = makeRelFile "resfile0"
resfileN = makeRelFile "resfileN"

--res0 = "LegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/minimal/minimal.text\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/minimal/minimal.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/panchatantra/The Panchatantra (Purnabhadra's Recension of 1199 CE).txt\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/panchatantra/pancha1_4000.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/panchatantra/pancha1.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/panchatantra/pancha1.txt\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/carol/test1.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/carol/test3.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/carol/test2.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/carol/minimal.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/carol/pg11.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/tawada/bad.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/tawada/Tawada-Fremde.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/tawada/Arufabetto.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/tawada/Auge.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/tawada/etueden.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/tawada/t3.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/tawada/t2.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/waterhouse/t3 (copy).markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/waterhouse/t1.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/waterhouse/t2b.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/waterhouse/t2utf_markup.txt\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/waterhouse/t2a.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/waterhouse/t3.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/waterhouse/t2.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/LaFontaine/7241test.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/LaFontaine/7241.txt\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/LaFontaine/7241.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/LaFontaine/7241xx.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/test/t1.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/test/test.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/test/pg12partial.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/test/in.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/test/t3.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/test/t4.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/test/t2.markup\"\n"
---- when only markup:
--resM = "LegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/minimal/minimal.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/panchatantra/pancha1_4000.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/panchatantra/pancha1.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/carol/test1.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/carol/test3.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/carol/test2.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/carol/minimal.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/carol/pg11.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/tawada/bad.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/tawada/Tawada-Fremde.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/tawada/Arufabetto.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/tawada/Auge.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/tawada/etueden.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/tawada/t3.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/tawada/t2.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/waterhouse/t3 (copy).markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/waterhouse/t1.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/waterhouse/t2b.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/waterhouse/t2a.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/waterhouse/t3.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/waterhouse/t2.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/LaFontaine/7241test.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/LaFontaine/7241.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/LaFontaine/7241xx.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/test/t1.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/test/test.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/test/pg12partial.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/test/in.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/test/t3.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/test/t4.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/test/t2.markup\"\n"

