-- #!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle

-----------------------------------------------------------------------------
--
-- Module      :  collect all markup files and process them
-- Copyright   :  andrew u frank -
--
-- | takes cmd line arguments and converts markup to nt
-- takes   cmd line argument: -s gives source dir fuer die markup files.
-- later : -f for the filename
-- later: language (to process the english files first and then the other languages
-- in turn

-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

import           Uniform.FileIO         hiding ((<>))

import           Uniform.Convenience.StartApp hiding ((<>) , (</>), (<.>))

import           Data.Semigroup               ((<>))
--import           Options.Applicative.Builder
import           Options.Applicative
import Producer.Servers
import LitTypes.TextDescriptor hiding ((<>))
import          Data.RDF.FileTypes (ntFileTriples,ntFileTriplesGZip)
import Processor.Main2sub (mainLitAndNLPproduction)

import Process.UtilsProcessing (processAll)
import Process.UtilsParseArgs (getArgsParsed, setDefaultOriginDir, selectServer
            , LitTextFlag (..), LitTextFlags )
import Processor.ProcessAll
--import qualified System.Directory as S (getHomeDirectory)

programName = "ntmake5 v 0.0.4.2" :: Text
progTitle = "produce the lit for all markup files " :: Text

main :: IO ()
main = startProg programName progTitle
            ( parseAndExecute
               (unlinesT ["gets the nt files" ])
               "dir relative to home"
            )
homeDirX = "/home/frank/" :: FilePath -- is now defined in fileio, but should be getting the
--            home dir automatically

parseAndExecute  :: Text -> Text ->  ErrIO ()
-- TODO change to use subparsers and store results in a record
-- arguments are very different from the store in jena?
parseAndExecute t1 t2    = do
--        homeDirX :: FilePath <- callIO $ S.getHomeDirectory
        args1 <- getArgsParsedNT t1 t2
        let args = setDefaultOriginDirNT args1 (addDir homeDirX ("gutenberg"::FilePath))
        putIOwords ["parseAndExecute: process all store", showT args]
        let server = selectServerNT args :: URI

--    putIOwords ["ntstore4 parseAndExecute all files", showT server, " from nt dir", showT ntdir ]

        let resultFile = makeAbsFile "/home/frank/ntstore4.txt" :: Path Abs File
        writeFileOrCreate2 resultFile ("" :: Text)  -- to make sure it exist
        -- not really interesting
--        let flags = LitTextFlags {flagForce = argForceFlag args
--                    , flagFrenchUD = argFrenchUDFlag args
--                    , flagIncludeText = False
--                    , flagDebug = argDebug args
--                    , flagXML = argXML args}
        let
             flags = args2flags args
             originDir = makeAbsDir  $  addDir homeDirX (argOrigin args) :: Path Abs Dir
             destinationDir = makeAbsDir $  addDir homeDirX ( argDestination args) :: Path Abs Dir
             authorReplacement = s2t . getNakedDir $ originDir
        createDirIfMissing' destinationDir
        putIOwords ["parseAndExecute: process store"
                    , "\n\tserver", showT server
                    , "\n\tdestinationDir", showT destinationDir
                    , "\n\tauthorReplacement",   authorReplacement
--                    , "\n\tforceFlag", showT forceFlag
--                    , "\n\tFrenchUDFlag", showT forceFlag
                    , "\n\tFlags", showT flags
--                    , "\n\tdbarg", showT dbarg
                    , "\n\toriginDir", showT originDir
                    ]
--        if null . argFn $ args
--            then
        processAll (processOneMarkup4  flags server authorReplacement destinationDir)
--            then processAll (putOneFile2 debugFlag forceFlag server  -- ntdir db
--                        dbarg mgraph )
                        isMarkup -- (\f -> isNT f || isGZ f)
                        originDir resultFile
--            else do
--                error "ntmake deals only with processing all markup files in origin directory"
----                putOneFile2 False forceFlag server dbarg mgraph fn
--                -- true for debug stores takes only the first 3 triples...
        return ()

setDefaultOriginDirNT :: NtmakeArgs -> String -> NtmakeArgs
setDefaultOriginDirNT args def = if null' $ argOrigin args then  args {argOrigin = def}
                                                             else args

selectServerNT :: NtmakeArgs -> URI
-- select the server between localhost and brest
selectServerNT args =  if  argLocalhost args
                    then serverLocalhost
                    else serverBrest  -- default


getArgsParsedNT :: Text -> Text -> ErrIO NtmakeArgs
getArgsParsedNT t1 t2 = do
        args <- callIO $ execParser opts
        return args
    where
        opts = info (helper <*> ntcmdArgs)
          ( fullDesc
         <> (progDesc . t2s $ t1)
         <> (header . t2s $ t2 ))

-- cmd line parsing -- cannot use utilsparse, because very different required fields
data NtmakeArgs = NtmakeArgs
  {
     argLocalhost :: Bool -- ^ use localhost as fuseki server
     , argDestination :: String -- ^ the directory for the nt files
   , argOrigin :: String -- ^ the directoy in which the markup files are
   , argForceFlag :: Bool -- ^ force processing, even if newer exist
   , argFrenchUDFlag :: Bool -- ^ used UD model for french
   , argDebug :: Bool -- ^ controls debug output
   , argXML :: Bool -- ^ controls output of xml as text
  } deriving (Show)

ntcmdArgs :: Parser (NtmakeArgs)
ntcmdArgs = NtmakeArgs
     <$> switch
          ( long "localhost" <>
            short 'l' <>
--            metavar "orig/test" <>
            help "localhost not serverBrest (default)" )
     <*> strOption
          ( long "destinationDir" <>
            short 'd' <>
            value "NT" <>
            metavar "Destination directory"
            <> help "directory for the nt files " )
     <*>  strOption
        (long "origin dir" <>
            short 'o'  <>
            value "" <>
            help "dir in which the markup or query files are (relative to home)" )
     <*> switch
          ( long "force" <>
            help "force processing even when newer exist (default false)" )
     <*> switch
          ( long "frenchUD" <>
            help "use for french the model trained for UD" )
     <*> switch
          ( long "debug" <>
            help "put debug output" )
     <*> switch
          ( long "XML" <>
            help "print xml" )

convertFlags2list :: NtmakeArgs -> LitTextFlags
-- convert the flags to a list of LitTextFlag values
convertFlags2list _ = []

flag2bool :: [(NtmakeArgs -> Bool)] -> NtmakeArgs -> [Bool]
flag2bool fs args = map (\f -> f args) fs

args2flags :: NtmakeArgs -> LitTextFlags
-- find the flags set to true and put into list of flags
args2flags args = selFlag [LocalNLPserverFlag, ForceFlag, DebugFlag, XMLflag]
                $ flag2bool [argLocalhost, argForceFlag, argDebug, argXML] args

selFlag :: [LitTextFlag] -> [Bool] -> [LitTextFlag]
selFlag [] _  = []
selFlag _ []  = []
selFlag (a:as) (b:bs)  = if b then a : (selFlag as bs)  else selFlag as bs



isMarkup :: Path Abs File -> Bool
isMarkup  = hasExtension (Extension "markup")
-- todo include in typedfiles - hasType ...

