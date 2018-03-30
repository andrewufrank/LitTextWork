 -----------------------------------------------------------------------------
--
-- Module      :  utils
-- Copyright   :  andrew u frank -
--
-- | utilities for the query and storage processing
-----------------------------------------------------------------------------
--{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables,
    DeriveGeneric, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}


module CmdLineUtilities.UtilsParseArgs
    (LitArgs (..), getArgsParsed
--            module CmdLineUtilities.UtilsParseArgs
--        , dirQueries, URI
    , module GHC.Generics
    , module Uniform.Error
--    , module Uniform.Zero
    )
    where

--import           Uniform.FileIO hiding ((<>), (</>), (<.>))
import           Uniform.Error hiding ((<>), (</>), (<.>))
--import Uniform.Zero
import           Data.Semigroup               ((<>))
import           Options.Applicative.Builder
import           Options.Applicative
import GHC.Generics
--import LitTypes.TextDescriptor (serverLocalhost, serverBrest
--            , rdfBase, dirQueries, URI)
--import Path.IO as Pathio

--data LitTextFlag = DebugFlag | ForceFlag | IncludeTextFlag
--            | OutputNLPflag | XMLflag | JSONflag
----            | LocalNLPserverFlag
--            | SnipSet Int
--            | NoFlagZero
--            deriving (Show, Read, Eq, Ord, Generic)
--
--data ServerFlag = LocalServer | RemoteServer
--            deriving (Show, Read, Eq, Ord, Generic)
--
--type LitTextFlags = [LitTextFlag]
--instance Zeros LitTextFlag where zero = NoFlagZero



-- check all filenames by converting to Path format

--- cmd line parsing
data LitArgs = LitArgs
  {
--    argOrigTest   :: Bool   -- ^ orig or test to decide where to take the file
    argLocalhost :: Bool -- ^ use localhost as fuseki server
--  , argQuery :: Bool -- ^ query to produce file, not an update
  , argDB :: String -- ^ the database name
  , argGraph  :: String  -- ^ the graph
  , argAuxGraph  :: String  -- ^ an additional graph
--  , argDest  :: String  -- ^ the destination graph
  , argSubDir  :: String  -- ^ the query dir
  , argFn  :: String -- ^ the query filename without extension
--  , argResultDir :: String -- ^ the result dir
  , argWordnetGraph  :: String -- ^ the name of the wordnet graph without <..>
  , argTimeout  :: String -- ^ the timeout (if any) in minutes
  , argOrigin :: String -- ^ the directoy in which the markup files are
  , argForceFlag :: Bool -- ^ force processing, even if newer exist
--  , argBookNrFile :: String -- ^ a single book (not a full link)

  } deriving (Show)

cmdArgs :: Parser LitArgs
cmdArgs = LitArgs
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
          ( long "database (was corpus)" <>
            short 'd' <>
        --   long "subdir" <>
            metavar "db (was corpus) - required " <>
            help "dbname from fuseki (was corpus)" )
     <*> strOption
          ( long "graph - required" <>
            short 'g' <>
            metavar "Graph" <>
            value "" <>
            help "graph - required" )
     <*> strOption
          ( long "auxGraph" <>
            short 'a' <>
            value "" <>
            metavar "Auxilliary Graph" <>
            help "graph" )
--     <*> strOption
--          ( long "dest" <>
--            short 'd' <>
--            value "" <>
--            metavar "Destination Graph" <>
--            help "destination graph" )
     <*> strOption
          ( long "subdir for queries or nt" <>
            short 's' <>
            value "" <>
            metavar "subdir (folder) with the construct queries" <>
            help "subdir (folder) with the construct queries to execute" )
     <*> strOption
          ( long "filename of construct query" <>
            short 'f' <>
            value "" <>
            metavar "query (filename)" <>
            help "filename (without .construct), will also be name of resulting .ttl" )
--     <*> strOption
--          ( long "result dir" <>
--            short 'r' <>
--            value "/home/frank/constructResults" <>
--            metavar "result dir" <>
--            help "filename (without .construct), will also be name of resulting .ttl" )
     <*> strOption
          ( long "wordnet" <>
            short 'w' <>
            value "" <> -- "http://nlp.gerastree.at:3030/corpus3/data/wn31" <>
            metavar "wordnet graph" <>
            help "the wordnet graph without <..> " )
     <*> strOption
          ( long "timeout" <>
            short 't' <>
            value "" <>
            metavar "timeout" <>
            help "timeout in minutes " )
     <*>  strOption
        (long "origin dir (relative to home)" <>
            short 'o'  <>
            value "" <>
            help "dir in which the nt?, markup or query files are \
                        \(relative to home)" )
     <*> switch
          ( long "force" <>
--            value False <>
--            short 'l' <>
--            metavar "force processing " <>
            help "force processing even when newer exist (default false)" )
--     <*>  strOption
--        (long "input file with the booknumbers " <>   -- must be synced manually to brest?
--            short 'i'  <>
--            value "" <> -- set to in main /home/frank/gutenberg" <>
--            help "input file with the booknumbers - cvs, booknr in first column" )

--setDefaultOriginDir :: LitArgs -> String -> LitArgs
---- is always homedir
--setDefaultOriginDir args def =
--        if null' $ argOrigin args
--            then  args {argOrigin = def}
--            else args

getArgsParsed :: Text -> Text -> ErrIO LitArgs
getArgsParsed t1 t2 = do
        args <- callIO $ execParser opts
        return args
    where
        opts = info (helper <*> cmdArgs)
          ( fullDesc
         <> (progDesc . t2s $ t1)
         <> (header . t2s $ t2 ))

--
