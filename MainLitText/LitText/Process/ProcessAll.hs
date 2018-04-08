 -----------------------------------------------------------------------------
--
-- Module      :  Process ProcessAll
-- Copyright   :  andrew u frank -
--
-- | finds all markup files and process them to store in triple store
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}


module LitText.Process.ProcessAll
    (module LitText.Process.ProcessAll
    , module LitText.Process.Main2sub
    , serverBrest
    ) where

import LitText.Process.Main2sub

import qualified Pipes as Pipe
import qualified Pipes.Prelude as Pipe
import Pipes ((>->), (~>))
-- todo fileio - export for pipes
import Data.List (delete)
import Uniform.FileIO
import LitText.Foundation
--         hiding (try, (<|>))

processOneMarkup4 ::  LitTextFlagSet -> Path Abs Dir
            -> Path Abs Dir -> Path Abs File
            -> ErrIO Text
-- process one markup file, if the nt file does not exist
processOneMarkup4  flags origindir ntdir   file = do
    putIOwords ["\n processOneMarkup4", showT flags
                , "\n\t originDir", showT origindir
                , "\n\t ntdir " , showT ntdir
                , "\n\t file", showT file]
    let buchReplacement = s2t $ getNakedFileName file
        flags2 = flags -- why delete? delete IncludeTextFlag flags
        nlpserver = getServer flags
        authorReplacement = s2t . getNakedDir $ origindir
        textstate2 = fillTextState4a file nlpserver ntdir
                        authorReplacement buchReplacement flags2
    -- forces gzip in fillTextState4a
    putIOwords ["\n processOneMarkup4", showT textstate2
                , " server", showT nlpserver  ]
    if  gzipFlag . ntdescriptor $ textstate2
        then do
            ntgzExist <- exist6
                        (destNT . ntdescriptor $ textstate2)
                        ntFileTriplesGZip
            processNeeded <-
                if ntgzExist
                then do
                    nttime <- modificationTime6
                            (destNT . ntdescriptor $ textstate2)
                            ntFileTriplesGZip
                    markuptime <- getFileModificationTime file
                    return $ nttime < markuptime
                else
                    return True

            if processNeeded || (isLocalServer flags2)
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
                    putIOwords  ["\n processOneMarkup4 - \
                                        \newer nt file exist already"
                            , showT $ sourceMarkup textstate2, "\n"]
                    return . unlinesT $ ["\n processOneMarkup4 - \
                                        \nt file exist already"
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


