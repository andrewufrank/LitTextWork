 -----------------------------------------------------------------------------
--
-- Module      :  Processor ProcessAll
-- Copyright   :  andrew u frank -
--
-- | finds all markup files and process them to store in triple store
-----------------------------------------------------------------------------
--{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}


module Processor.ProcessAll
    (module Processor.ProcessAll
    , module Parser.TextDescriptor
    , module Processor.Main2sub
    , serverBrest
    ) where

--import           Test.Framework

import Parser.TextDescriptor hiding ((<>) , (</>), (<.>))
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
import          Data.RDF.FileTypes (ntFileTriples,ntFileTriplesGZip)
import Process.UtilsParseArgs ( LitTextFlags (..) )

--processAll :: Bool ->  LitDirs-> URI -> Path ar File  -> ErrIO ()
---- | get all markup files in the partially filled TextState2
---- the output goes to the second, not the triples
---- debug flag is not yet used
--
--processAll debug litdirs  server file  =  do
--  let path = source litdirs
--  resFile <- makeAbsolute file
--  bracketErrIO (openFile2handle resFile WriteMode)
--        (closeFile2)
--        (\hand -> do
--              Pipe.runEffect $
--                getRecursiveContents path
--                >-> Pipe.filter isMarkup --
--                >-> Pipe.mapM (fmap t2s . processOneMarkup debug litdirs server)
--            --    >-> P.stdoutLn
--                >-> Pipe.toHandle hand
--        )
--
--isMarkup :: Path Abs File -> Bool
--isMarkup  = hasExtension (Extension "markup")
---- todo include in typedfiles - hasType ...
--
----debugNLP = False
----litDebugOnly = False

processOneMarkup4 ::  LitTextFlags ->  URI -> Text -> Path Abs Dir -> Path Abs File
            -> ErrIO Text
-- process one markup file, if the nt file does not exist
processOneMarkup4  flags  server authorReplacement ntdir   file = do
    let buchReplacement = s2t $ getNakedFileName file
        flags2 = flags {flagIncludeText = False}
        textstate2 = fillTextState4a file server ntdir authorReplacement buchReplacement flags2
    -- forces gzip in fillTextState4a
    putIOwords ["\n processOneMarkup", showT textstate2  ]
    if  gzipFlag . ntdescriptor $ textstate2
        then do
            ntgzExist <- exist6 (destNT . ntdescriptor $ textstate2) ntFileTriplesGZip
            processNeeded <-
                if ntgzExist
                then do
                    nttime <- modificationTime6 (destNT . ntdescriptor $ textstate2) ntFileTriplesGZip
                    markuptime <- getFileModificationTime file
                    return $ nttime < markuptime
                else
                    return True

            if processNeeded || (flagForce flags2)
                then do
                    putIOwords  ["\n processOneMarkup4 - process"
                            , showT $ sourceMarkup textstate2, "\n"]
                    mainLitAndNLPproduction flags2 textstate2
                    -- first bool is debug output
                    -- second if true stops processing after lit (no nlp calls)
                    putIOwords  ["\n processOneMarkup4 - processed"
                            , showT $ sourceMarkup textstate2, "\n"]
                    return (showT textstate2)
                else do
                    putIOwords  ["\n processOneMarkup4 - newer nt file exist already"
                            , showT $ sourceMarkup textstate2, "\n"]
                    return . unlinesT $ ["\n processOneMarkup4 - nt file exist already"
                            , showT $ sourceMarkup textstate2, "\n"]

        else do -- not gzipflag  -- not expected anymore
            putIOwords  ["\n processOneMarkup4 - not gzip", "\n"]
            error "not gzip in processOneMarkup4 "
            return "not gzip"

    `catchError` \e -> do
            putIOwords  ["\n processOneMarkup4 - error return for "
                    , showT file, "\n"
                    , "error", e
                    , "not raised further to continue processing all"
                    ]
            return ""

--processOneMarkup :: Bool ->  LitDirs -> URI -> Path Abs File
--            -> ErrIO Text
---- process one markup file, if the nt file does not exist
--processOneMarkup debug  litDirs server lfp = do
--        let textstate2 = fillTextState4 litDirs server lfp
--        putIOwords ["\nprocessOneMarkup", showT server  ]
--        ntExist <- exist6 (destNT textstate2) ntFileTriples
--        if not ntExist
--            then do
--                putIOwords  ["\nprocessMarkup - process"
--                        , showT $ sourceMarkup textstate2, "\n"]
--                mainLitAndNLPproduction False False textstate2
--                -- first bool is debug output
--                -- second stops processing after lit (no nlp calls)
--                putIOwords  ["\nprocessMarkup - processed"
--                        , showT $ sourceMarkup textstate2, "\n"]
--                return (showT textstate2)
--            else do
--                putIOwords  ["\nprocessMarkup - nt file exist already"
--                        , showT $ sourceMarkup textstate2, "\n"]
--                return . unlinesT $ ["\nprocessMarkup - nt file exist already"
--                        , showT $ sourceMarkup textstate2, "\n"]
--
--    `catchError` \e -> do
--            putIOwords  ["\nprocessMarkup - error return for "
--                    , showT lfp, "\n"
--                    , "error", e
--                    , "not raised further to continue processing all"
--                    ]
--            return ""


--fillTextState :: TextSource -> DestGenerality -> Path Abs File -> TextState2
--fillTextState ts dg fp = fillTextState2 ts dg author buch
--    where
--        author = getImmediateParentDir fp
--        buch = getNakedFileName fp




