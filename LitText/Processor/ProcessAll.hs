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
    ) where

import           Test.Framework
import           Uniform.Error           (errorT)
import           Uniform.Strings         hiding ((<|>))
import Uniform.FileIO
-- todo hasExtension
import Parser.Foundation

import qualified Pipes as Pipe
import qualified Pipes.Prelude as Pipe
import Pipes ((>->), (~>))


import Uniform.Error
import Uniform.FileIO
import Uniform.FileStatus
import Uniform.Strings hiding ((</>))


--import Pipes
--import qualified Pipes.Prelude as P
--import Control.Monad (forM_)
--import System.Directory (doesDirectoryExist, getDirectoryContents)
--import System.Environment (getArgs)
--import System.FilePath ((</>))

--getRecursiveContents :: FilePath -> Pipe.Producer FilePath IO ()
--getRecursiveContents topPath = do
--  names <- lift $ getDirectoryContents topPath
--  let properNames = filter (`notElem` [".", ".."]) names
--  forM_ properNames $ \name -> do
--    let path = topPath </> name
--    isDirectory <- lift $ doesDirectoryExist path
--    if isDirectory
--      then getRecursiveContents path
--      else yield path


processAll :: TextState2 ->   LegalPathname  -> ErrIO ()
-- | get all markup files in the directory given
-- the result is in the first argument file
processAll textstate0  file  =  do
  let path = originalsDir textstate0
  hand <-   openFile file WriteMode
  Pipe.runEffect $
    getRecursiveContents path
    >-> Pipe.filter isMarkup --
    >-> Pipe.mapM (fmap t2s . processOneMarkup textstate0)
--    >-> P.stdoutLn
    >-> Pipe.toHandle hand
  closeFile2 hand

isMarkup = hasExtension "markup"

--filterMarkup :: [LegalPathname] -> [LegalPathname]
--filterMarkup   = filter (hasExtension "markup")

processOneMarkup :: TextState2 -> LegalPathname -> ErrIO Text
processOneMarkup textstate0 lfp = do
    let textstate2 = fillTextState textstate0 lfp
    putIOwords [showT textstate2]
    return (showT textstate2)

fillTextState :: TextState2 -> LegalPathname -> TextState2
fillTextState textState0 fp = textState0 {  -- enthaelt endpotin, originalsDir
--                            authorDir = filename2text fp a
                              buchname = filename2text fp f
--                            , graph = filename2text fp a   -- the graph is the same as the author
                            }
    where
        (p,f,e) = splitFilepath fp
        a = last . splitDirectories $ p


    ----------------- tests
origDirForTest = "/home/frank/additionalSpace/DataBig/LitTest" :: Text
-- /home/frank/additionalSpace/DataBig/LitTest/carol


textstate0 = TextState2 {endpoint = ""
        , serverLoc = ""
        , originalsDir = mkFilepath lpX origDirForTest
        , authorDir = ""
        , buchname = ""
        , graph = ""
        }

test_0 = do
    runErr $ do
--        let origDir = mkFilepath lpX origDirForTest
        let resFile = mkFilepath lpX "test0dir"

        processAll textstate0 resFile
    res <- readFile "test0dir"
    assertEqual resM res

res0 = "LegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/minimal/minimal.text\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/minimal/minimal.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/panchatantra/The Panchatantra (Purnabhadra's Recension of 1199 CE).txt\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/panchatantra/pancha1_4000.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/panchatantra/pancha1.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/panchatantra/pancha1.txt\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/carol/test1.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/carol/test3.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/carol/test2.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/carol/minimal.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/carol/pg11.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/tawada/bad.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/tawada/Tawada-Fremde.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/tawada/Arufabetto.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/tawada/Auge.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/tawada/etueden.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/tawada/t3.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/tawada/t2.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/waterhouse/t3 (copy).markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/waterhouse/t1.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/waterhouse/t2b.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/waterhouse/t2utf_markup.txt\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/waterhouse/t2a.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/waterhouse/t3.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/waterhouse/t2.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/LaFontaine/7241test.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/LaFontaine/7241.txt\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/LaFontaine/7241.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/LaFontaine/7241xx.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/test/t1.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/test/test.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/test/pg12partial.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/test/in.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/test/t3.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/test/t4.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/test/t2.markup\"\n"
-- when only markup:
resM = "LegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/minimal/minimal.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/panchatantra/pancha1_4000.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/panchatantra/pancha1.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/carol/test1.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/carol/test3.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/carol/test2.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/carol/minimal.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/carol/pg11.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/tawada/bad.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/tawada/Tawada-Fremde.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/tawada/Arufabetto.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/tawada/Auge.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/tawada/etueden.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/tawada/t3.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/tawada/t2.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/waterhouse/t3 (copy).markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/waterhouse/t1.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/waterhouse/t2b.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/waterhouse/t2a.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/waterhouse/t3.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/waterhouse/t2.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/LaFontaine/7241test.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/LaFontaine/7241.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/LaFontaine/7241xx.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/test/t1.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/test/test.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/test/pg12partial.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/test/in.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/test/t3.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/test/t4.markup\"\nLegalPathname \"/home/frank/additionalSpace/DataBig/LitTest/test/t2.markup\"\n"

