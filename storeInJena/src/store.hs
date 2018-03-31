-- #!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle

-----------------------------------------------------------------------------
--
-- Module      :  store an nt file in jena fuseki sparql update
-- Copyright   :  andrew u frank -
--
-- | store all nt and nt.gz files from a directory (for using with gutenberg catalog selected files)
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Uniform.Convenience.StartApp hiding ((<>) , (</>), (<.>))
import           Uniform.FileIO hiding ((<>) ,   (<.>))

import Process.StoreOneFile
import LitTypes.TextDescriptor (ntDirsRel, dirsTest, dirsOrig, LitDirs(..))

import CmdLineUtilities.UtilsProcessCmd
import CmdLineUtilities.UtilsProcessing
--import Process.UtilsParseArgs
--import Producer.Servers (ntDirsRel)

programName = "store5" :: Text
progTitle = "store nt, nt.gz or ttl files in fuseki jena" :: Text


main =  do
    startProg programName progTitle
        (parseAndExecute
           progTitle
        "localhost/brest, corpus, graph, nt directory (relative to /home/frank) - if none given, all NT dir and subdir are searched"
        )
    return ()


----- cmd line parsing generalized in process.ParseArgs
--  possibly the argument parsing is too general  split in list and sublist for individual ops

--homeDir = makeAbsDir "/home/frank/"  -- now in uniform.filenames

parseAndExecute  :: Text -> Text ->  ErrIO ()
parseAndExecute t1 t2    = do
    inp <- parseAndStartExecute True "storeReport.txt"  t1 t2
    homeDirz <- homeDir2
    let resultFile = addFileName homeDirz ("resultFileStore6.txt" ::FilePath)
                             :: Path Abs File
--    let args = inArgs inp
    when (isZero . inDB $ inp) $
            errorT ["parseAndExecute: process store - requires base (dataset)"]
    when (isNothing . inGraph $ inp) $
            errorT ["parseAndExecute: process store - requires graph "]
    when (isZero . showT . inOriginDir $ inp) $
            errorT ["parseAndExecute: process store - requires origin (source) dir "]

    case inFilename inp of
        Nothing -> do
            putIOwords ["parseAndExecute: process store - no Fn (filename) argument)"]
            if null . inFilename $ inp
                then do
                    putIOwords ["parseAndExecute: process store"
                        , " - no BookNr argument  - process all"
                        , "for", showT $ inOriginDir inp]
--                            let originSub = if arg
--                            processAll (putOneFile2 debugFlag forceFlag server  -- ntdir db
--                                dbarg mgraph ) (\f -> isNT f || isGZ f) originDir resultFile
                    processAll (putOneFile4 inp)  (\f -> isNT f || isGZ f)
                                (inOriginDir inp) resultFile
                else do
                    putIOwords ["parseAndExecute: process store - with BookNr"
--                                    , s2t . argBookNrFile $ args]
                                , "storing with booknr is not implemented anymore"]
--                    let csvfilename = addFileName homeDir (inFilename inp)
--                    putIOwords ["parseAndExecute: process a csv file with book numbers"
--                                    , showT csvfilename]
--                    putFilesFromCVS3  inp csvfilename
                    return ""
        Just fn -> do
            putIOwords ["parseAndExecute: process store  - with filename)"
                    ]
            putIOwords ["parseAndExecute: completed filename", showT fn]
            -- fails if fn is null
            putOneFile4 inp fn
    return ()

isNT :: Path Abs File -> Bool
isNT  = hasExtension ntExtension

isGZ :: Path Abs File -> Bool
isGZ = hasExtension (Extension "gz")

