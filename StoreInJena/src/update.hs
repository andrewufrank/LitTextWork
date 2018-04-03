 -----------------------------------------------------------------------------
--
-- Module      :  AllCaps
-- Copyright   :  andrew u frank -
--
-- | processes all updates in a subfolder
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}


import           Uniform.Convenience.StartApp hiding ((<>) , (</>), (<.>))
import           Uniform.FileIO hiding ((<>) ,   (<.>))

import           Data.Semigroup               ((<>))
import           Options.Applicative.Builder
import           Uniform.Convenience.StartApp hiding ((<>) , (</>), (<.>))
import           Options.Applicative

import qualified Pipes as Pipe
import qualified Pipes.Prelude as Pipe
import Pipes ((>->), (~>))

import Process.OneUpdate
import Process.OneQuery
import Producer.Servers (serverLocalhost, serverBrest, rdfBase, dirQueries, URI)

programName = "allUpdates" :: Text
progTitle = "process all updates in a folder " :: Text


main =  do
--    let fn = "/home/frank/additionalSpace/DataBig/LitTest/doyle/2852-0-short1"
    startProg programName progTitle
        (parseAndExecute
           (unlinesT ["process the updates or queries in a folder" ]
           )
        "corpus, graph, destination graph, folder, wordnet graph"
        )
    return ()


--- cmd line parsing
data LitArgs = LitArgs
  {
--    argOrigTest   :: Bool   -- ^ orig or test to decide where to take the file
    argLocalhost :: Bool -- ^ use localhost as fuseki server
  , argQuery :: Bool -- ^ query to produce file, not an update
  , argDB :: String -- ^ the database name
  , argGraph  :: String  -- ^ the graph
  , argAuxGraph  :: String  -- ^ an additional graph
  , argDest  :: String  -- ^ the destination graph
  , argDir  :: String  -- ^ the query dir
  , argFn  :: String -- ^ the update filename without extension
  , argWordnetGraph  :: String -- ^ the name of the wordnet graph without <..>
  } deriving (Show)

cmdArgs :: Parser (LitArgs)
cmdArgs = LitArgs
     <$> switch
          ( long "localhost" <>
            short 'l' <>
--            metavar "orig/test" <>
            help "localhost not serverBrest" )
     <*> switch
          ( long "query" <>
            short 'q' <>
--            metavar "orig/test" <>
            help "query (not update)" )
     <*> strOption
          ( long "corpus" <>
            short 'c' <>
        --   long "subdir" <>
            metavar "corpus(DB)" <>
            help "corpus=dbname" )
     <*> strOption
          ( long "graph" <>
            short 'g' <>
            metavar "Graph" <>
            help "graph - required even if not really used" )
     <*> strOption
          ( long "auxGraph" <>
            short 'a' <>
            value "" <>
            metavar "Auxilliary Graph" <>
            help "graph" )
     <*> strOption
          ( long "dest" <>
            short 'd' <>
            value "" <>
            metavar "Destination Graph" <>
            help "destination graph" )
     <*> strOption
          ( long "subdir" <>
            short 's' <>
--            value "" <>
            metavar "subdir (folder) with the updates" <>
            help "subdir (folder) with the queries to execute" )
     <*> strOption
          ( long "filename" <>
            short 'f' <>
    value "" <>
            metavar "query filename" <>
            help "query name (with or without .update or .query)" )
     <*> strOption
          ( long "wordnet" <>
            short 'w' <>
            value "http://nlp.gerastree.at:3030/corpus3/data/wn31" <>
            metavar "wordnet graph" <>
            help "the wordnet graph without <..> " )


parseAndExecute  :: Text -> Text ->  ErrIO ()
parseAndExecute t1 t2    = do
        args <- callIO $ execParser opts
        let server = if  argLocalhost args
                    then serverLocalhost
                    else serverBrest
        let queryDir =  addDir  (dirQueries :: Path Abs Dir) (argDir args)
                    ::Path Abs Dir
        if  null (argFn args)
            then do
                putIOwords ["parseAndExecute: process all updates", showT args]
                processAllUQ True server(s2t $ argDB args)  -- deals only with updates? queries?
                    (Just . s2t . argGraph $ args)
                    (Just . s2t . argDest $ args)
                    (s2t . argWordnetGraph $ args) queryDir
          else
            do
                let  fn0 = addFileName (dirQueries :: Path Abs Dir)
                             (argDir args </>   (argFn args :: FilePath) )
                                    :: Path Abs File
                if argQuery args
                    then oneQuery True server (s2t $ argDB args)
                            (Just . s2t $ argGraph args)
                            (Just . s2t $ argAuxGraph args)
                            fn0
                    else oneUpdate True server (s2t $ argDB args)
                            (Just . s2t $ argGraph args)
                            (Just . s2t . argDest $ args)
                            (s2t . argWordnetGraph $ args) fn0
--                  (makeAbsDir $ argDir args)
                return ()
      where
        opts = info (helper <*> cmdArgs)
          ( fullDesc
         <> (progDesc . t2s $ t1)
         <> (header . t2s $ t2 ))

processAllUQ :: Bool ->  URI -> Text  -> Maybe Text -> Maybe Text -> Text
        ->  Path Abs Dir  -> ErrIO ()
-- | in the db use graph1 and produce dest graph with query in dir
-- nothing goes to default graph
-- debug flag is not yet used
-- process upadet and queries (not yet)

processAllUQ debug server db mgraph mdest wordnetgraph dir  =  do
  putIOwords ["processAllUQ: process all updates", showT dir]
  let resFile = addFileName dir ("resultFile.txt" :: FilePath) :: Path Abs File
  putIOwords ["processAllUQ: process all updates", showT dir]
  bracketErrIO (openFile2handle resFile WriteMode)
    (closeFile2)
    (\hand -> do
      Pipe.runEffect $
        getRecursiveContents dir
        >-> Pipe.filter isUpdate--
        >-> Pipe.mapM (fmap t2s . oneUpdate debug server db mgraph mdest wordnetgraph)
    --    >-> P.stdoutLn
        >-> Pipe.toHandle hand
    )

isUpdate :: Path Abs File -> Bool
isUpdate  = hasExtension (Extension "update")
-- todo include in typedfiles - hasType ...
