 -----------------------------------------------------------------------------
--
-- Module      :  AllCaps
-- Copyright   :  andrew u frank -
--
-- | processes queries, not update (mostly
-- queries are in dirQueries = makeAbsDir "/home/frank/additionalSpace/DataBig/Queries"
-- / defined in foundattion
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

--import           Data.Semigroup               ((<>))
--import           Options.Applicative.Builder
--import           Options.Applicative

import qualified Pipes as Pipe
import qualified Pipes.Prelude as Pipe
import Pipes ((>->), (~>))

--import Process.OneUpdate
import Process.OneConstruct
import LitTypes.TextDescriptor ( dirQueriesRel)
--import Process.UtilsParseArgs
import LitTypes.UtilsProcessing

programName = "construc4" :: Text
progTitle = "process construct queries (single or all in a folder)" :: Text

main =  do
    startProg programName progTitle
        (parseAndExecute
           (unlinesT ["process the construct queries in a folder" ]
           )
        "corpus, graph, aux/wordnet graph, query "
        )
    return ()

parseAndExecute  :: Text -> Text ->  ErrIO ()
parseAndExecute t1 t2    = do
    inp <- parseAndStartExecute True "constructReport.txt" ( dirQueriesRel) t1 t2
    let args = inArgs inp
    case inFilename inp of
        Nothing -> do
                putIOwords ["parseAndExecute: process all constructs", showT args]
                res <- processAll (oneConstruct2 inp) isConstruct (inOriginDir inp) (inResultFile inp)
                putIOwords ["parseAndExecute: process all constructs result", res]
                return ()
        Just fn -> do
                let queryFile = getFn args
                    timeout = getTimeout args
                    graph = Just . s2t $ argGraph args
                    dbarg = (s2t $ argDB args)  :: Text
                oneConstruct2 inp queryFile
                return ()


isConstruct :: Path Abs File -> Bool
isConstruct  = hasExtension (Extension "construct")
-- todo include in typedfiles - hasType ...
