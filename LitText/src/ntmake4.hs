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

--import qualified Pipes as Pipe
--import qualified Pipes.Prelude as Pipe
--import Pipes ((>->), (~>))

--import Processor.ProcessAll hiding ((<>) , (</>), (<.>))
--import           Parser.Foundation ()
--     hiding ((<>) , (</>), (<.>))
-- import           Parser.LinesToParagrahs
import           Uniform.FileIO         hiding ((<>))
--import           Uniform.Strings              hiding ((<>), (</>), (<.>))
--import Lines2para.Lines2ignore (LanguageCode, readLanguageCode)
--            hiding ((<>) , (</>), (<.>))

import           Uniform.Convenience.StartApp hiding ((<>) , (</>), (<.>))

import           Data.Semigroup               ((<>))
import           Options.Applicative.Builder
import           Options.Applicative
import Producer.Servers
import Parser.TextDescriptor hiding ((<>))
import          Data.RDF.FileTypes (ntFileTriples,ntFileTriplesGZip)
import Processor.Main2sub (mainLitAndNLPproduction)

import Process.UtilsProcessing (processAll)
import Process.UtilsParseArgs (getArgsParsed, setDefaultOriginDir, selectServer)
import Processor.ProcessAll
import qualified System.Directory as S (getHomeDirectory)

programName = "ntmake" :: Text
progTitle = "produce the lit for all markup files " :: Text

main :: IO ()
main = startProg programName progTitle
            ( parseAndExecute
               (unlinesT ["gets the nt files" ])
               "dir relative to home"
            )


parseAndExecute  :: Text -> Text ->  ErrIO ()
parseAndExecute t1 t2    = do
        homeDir :: FilePath <- callIO $ S.getHomeDirectory
        args1 <- getArgsParsedNT t1 t2
        let args = setDefaultOriginDirNT args1 (addDir homeDir ("gutenberg"::FilePath))
        putIOwords ["parseAndExecute: process all store", showT args]
        let server = selectServerNT args :: URI
--    args <- callIO $ execParser opts
--    let homedir = makeAbsDir "/home/frank/"
--    putIOwords ["ntstore4 parseAndExecute dir", showT args]
--    let server = if  argLocalhost args
--                    then serverLocalhost
--                    else serverBrest
--        let ntdir =   (makeAbsDir $ argOrigin args) :: Path Abs Dir

--    putIOwords ["ntstore4 parseAndExecute all files", showT server, " from nt dir", showT ntdir ]

        let resultFile = makeAbsFile "/home/frank/ntstore4.txt" :: Path Abs File
        writeFileOrCreate2 resultFile ("" :: Text)  -- to make sure it exist
        -- not really interesting
        let forceFlag = argForceFlag args
        let debugFlag = False
--        let dbarg = s2t . argDB $ args
--        let mgraph = Just . s2t $ argGraph args
--        let fn = getFn args -- addFileName homedir  (makeRelFile $ argFn args) :: Path Abs File
        let
--             timeout = getTimeout args
--             mgraph = Just . s2t $ argGraph args
--             dbarg = s2t $ argDB args
             originDir = makeAbsDir  $  addDir homeDir (argOrigin args) :: Path Abs Dir
             destinationDir = makeAbsDir $  addDir homeDir ( argDestination args) :: Path Abs Dir
             authorReplacement = s2t . getNakedDir $ originDir
        createDirIfMissing' destinationDir
        putIOwords ["parseAndExecute: process store"
                    , "\n\tserver", showT server
                    , "\n\tdestinationDir", showT destinationDir
                    , "\n\tauthorReplacement",   authorReplacement
                    , "\n\tforceFlag", showT forceFlag
--                    , "\n\tdbarg", showT dbarg
                    , "\n\toriginDir", showT originDir
                    ]
--        if null . argFn $ args
--            then
        processAll (processOneMarkup4 debugFlag forceFlag server authorReplacement destinationDir)
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
--   , argSource :: String -- ^ the directoy in which the markup files are
--  , argdir   :: String   -- ^ the subdirectory in originals
--                        -- where the markup file is
--                        -- the same dirname is used in convertsDir
--  , argbuch  :: String -- ^ the filename in the dir
   , argOrigin :: String -- ^ the directoy in which the markup files are
   , argForceFlag :: Bool -- ^ force processing, even if newer exist
  } deriving (Show)

ntcmdArgs :: Parser (NtmakeArgs)
ntcmdArgs = NtmakeArgs
     <$> switch
          ( long "localhost" <>
            short 'l' <>
--            metavar "orig/test" <>
            help "localhost not serverBrest (default)" )
--     <*> switch
--          ( long "query" <>
--            short 'q' <>
----            metavar "orig/test" <>
--            help "query (not update)" )
     <*> strOption
          ( long "destinationDir" <>
            short 'd' <>
            value "" <>
            metavar "Destination directory"
            <> help "directory for the nt files " )
--     <*> strOption
--        (long "sourcedir" <>
--            short 's'  <>
--            help "dir in which the markup files are (relative to origin dir)" )
--      <*> switch
--          ( long "localhost" <>
--            short 'l' <>
----            metavar "orig/test" <>
--            help "localhost or serverBrest" )
--     <*> strOption
--          ( long "buch(file)" <>
--            short 'f' <>
--            value "" <>
--            metavar "buch"
--            <> help "buch - filename " )
     <*>  strOption
        (long "origin dir" <>
            short 'o'  <>
            value "" <>
            help "dir in which the markup or query files are (relative to home)" )
     <*> switch
          ( long "force" <>
--            value False <>
--            short 'l' <>
--            metavar "force processing " <>
            help "force processing even when newer exist (default false)" )



--parseAndExecute  :: Text -> Text ->  ErrIO ()
--parseAndExecute t1 t2    = do
--        args <- callIO $ execParser opts
----        let lang = readLanguageCode "readLangCode in MainCollect"
----                    (s2t $ argLanguage args) :: LanguageCode
--
--        let homedir = makeAbsDir "/home/frank" :: Path Abs Dir
--        let markupdir = addDir homedir (argSource args :: FilePath) :: Path Abs Dir
--        let resfile  = addFileName homedir $ makeRelFile "resultCollect" :: Path Abs File
--        let ntdir = addDir homedir (argTarget args) :: Path Abs Dir
--        let authorReplacement = s2t . getNakedDir . argSource $ args
--        processAll4 False markupdir serverBrest ntdir resfile authorReplacement
----
----        let server = if  argLocalhost args
----                    then serverLocalhost
----                    else serverBrest
----        if  null (argdir args)
----            then
----                processAll False  dirs  server  resfile
----            else do
----                let textstate = fillTextState3 dirs server
----                         (argdir args) (argbuch args)
----                mainLitAndNLPproduction False False textstate
--                -- first bool is debug output
--                -- second stops processing after lit (no nlp calls)
----                return ()
----        processAll False dir (s2t . argDB $ args) (Just . s2t $ argGraph args) resultFile
----        -- true for debug stores only the first 3 triples...
--        return ()

--processAll4 :: Bool ->  Path Abs Dir-> URI -> Path Abs Dir  -> Path Abs File  -> Text -> ErrIO ()
---- | get all markup files in the partially filled TextState2
---- the output goes to the second, not the triples
---- debug flag is not yet used
--
--processAll4 debug path  server ntdir resfile authorReplacement =
----  let path = source litdirs
--  bracketErrIO (openFile2handle resfile WriteMode)
--        (closeFile2)
--        (\hand ->
--              Pipe.runEffect $
--                getRecursiveContents path
--                >-> Pipe.filter isMarkup --
--                >-> Pipe.mapM (fmap t2s . processOneMarkup4 debug server ntdir authorReplacement)
--            --    >-> P.stdoutLn
--                >-> Pipe.toHandle hand
--        )

isMarkup :: Path Abs File -> Bool
isMarkup  = hasExtension (Extension "markup")
-- todo include in typedfiles - hasType ...

