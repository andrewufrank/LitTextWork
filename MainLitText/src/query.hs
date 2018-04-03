 -----------------------------------------------------------------------------
--
-- Module      :  AllCaps
-- Copyright   :  andrew u frank -
--
-- | processes queries, not update and store result as csv
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
--import           Options.Applicative.Builder
import           Uniform.Convenience.StartApp hiding ((<>) , (</>), (<.>))
--import           Options.Applicative

import qualified Pipes as Pipe
import qualified Pipes.Prelude as Pipe
import Pipes ((>->), (~>))

--import Process.OneUpdate
import Process.OneQuery
import LitTypes.TextDescriptor (serverLocalhost, serverBrest
            , rdfBase, dirQueries, dirQueriesRel, URI)
import LitTypes.UtilsProcessing
--import Process.UtilsParseArgs

programName = "queries" :: Text
progTitle = "process construct queries (single or all in a folder) " :: Text


main =  do
    startProg programName progTitle
        (parseAndExecute
           (unlinesT ["process the select queries in a folder" ]
           )
        "corpus, graph, destination graph, folder, wordnet graph"
        )
    return ()


parseAndExecute  :: Text -> Text ->  ErrIO ()
parseAndExecute t1 t2    = do
    inp <- parseAndStartExecute True "queryReport.txt" dirQueriesRel t1 t2
    let args = inArgs inp
--        args1 <- getArgsParsed t1 t2
--        let args = setDefaultOriginDir args1 (toFilePath dirQueriesRel)
--        putIOwords ["parseAndExecute: process all constructs", showT args]
--        let server = selectServer args :: URI
--        do
--                let dbarg = (s2t $ argDB args) :: Text
    let queryFile = getFn (inArgs inp) :: Path Abs File
--                    timeout = getTimeout args :: Maybe Int
--                    graph = Just . s2t $ argGraph args :: Maybe Text
--                    auxgraph = Just . s2t $ argAuxGraph args :: Maybe Text
--                putIOwords ["parseAndExecute: process query"
--                             , "\n\tserver",showT server
--                             , "\n\tdbarg", showT dbarg
--                             , "\n\tgraph", showT graph
--                             , "\n\tgraph", showT auxgraph
--                             , "\n\ttimeout", showT timeout
--                             , "\n\tqueryFile", showT queryFile
--
--                            ]
--
--    oneQuery True server dbarg graph auxgraph--  (s2t $ argDB args)
--                            timeout queryFile  -- no force flag
    oneQuery2 inp queryFile  -- no force flag
    return ()

