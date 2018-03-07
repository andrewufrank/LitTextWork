---------------------------------------------------------------------------
--
-- Module      :  Main2sub
-- Copyright   :  andrew u frank -
--
-- |  the reading the markup files and
-- removing characters not pertaining
-----------------------------------------------------------------------------
--{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}

module Parser.ReadMarkupAB
    (module Parser.ReadMarkupAB
        ) where

--import           Test.Framework
--import Uniform.TestHarness

import           LitTypes.TextDescriptor
import          LitTypes.ServerNames hiding ((<>) , (</>), (<.>))
import           Uniform.FileIO
import Uniform.TestHarnessUtilities.Utils

--testDir = makeAbsDir ("/home/frank/additionalSpace/DataBig/LitTest")
--serverLocTest = serverBrest --


data Markup
-- just a marking for a file type

markupFileType5 = makeTyped (Extension "markup") :: TypedFile5 Text Markup

instance TypedFiles5 Text Markup  where
    -- files of a single text stream, with a markup extension
--    mkTypedFile5  = TypedFile5 { tpext5 = Extension "markup"
--                    -- , parserF = tparser
--                    -- , writerF = twriter
--            }
--    write5 fp fn tp  ct = do
--        dirx <- ensureDir fp
--        let fn2 = fn <.> tpext5 tp -- :: Path ar File
--        writeFile2 (fp </> fn2 ) ct
    read5 fp fn tp   = do
        let fn2 = fn <.> (tpext5 tp)
        readFile2 (fp </> fn2)
    read6 fp tp   =  readFile2 (fp <.> tpext5 tp)
    write6 fp   tp  ct = do
--        dirx <- ensureDir fp
        let fp2 = fp <.> tpext5 tp -- :: Path ar File
        writeFile2 fp2 ct

debugRead = False
-- main export
textstate2Text :: TextDescriptor -> ErrIO Text  -- test A -> B
-- reads the markup file and converts to coded llines
textstate2Text textstate = do
    t <- readMarkupFile textstate -- test A -> B
    when debugRead $
        putIOwords ["textstate2TZ - the file content\n", t,
                         "\n with show\n", showT t, "\nend of file"]
    let t2 = filterChar (`notElem` ['\r']) t
    return t2

readMarkupFile :: TextDescriptor -> ErrIO Text
readMarkupFile textstate = do
    text <- read6 (sourceMarkup textstate) markupFileType5
    putIOwords ["readMarkupFile file", showT (sourceMarkup textstate)]

    bomWarning text   -- the check for BOM is in MainParse only -
    putIOwords ["text"] --, text]
    let text2 = s2t . filter (/='\SUB') . convertLatin . t2s $ text
    let text2 = s2t . filter (/= '\65279') . t2s $  text -- s2t . filter (/='\SUB') . convertLatin . t2s $ text
    -- to remove the non-latin characters which were not mapped
    --    and the \sub character if any present
    putIOwords ["text2"] --, text2]
    let nonlats =  nubChar . findNonLatinCharsT $ text2
--    putIOwords ["nonlats", nonlats]
    unless (null' nonlats) $
                putIOwords ["this file contains characters not in the latin1"
                , " charset which are not yet mapped\n"
                 , nonlats, showT nonlats]
    return text2

bomWarning :: Text -> ErrIO ()
bomWarning v = do  -- not required - parser filter it out
    -- putIOwords ["bomWarning - the start of the file "]
    -- putIOwords [take' 20 v]
    -- putIOwords [take' 20 $ showT v]
    when (isPrefixOf' "\65279" v) $
        putIOwords ["WARNING -- BOM character present - use ./unbom"
                    , "possibly already removed in the parsing"]
    return ()



