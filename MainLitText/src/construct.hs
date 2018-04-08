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

--import qualified Pipes as Pipe
--import qualified Pipes.Prelude as Pipe
--import Pipes ((>->), (~>))

--import Process.OneUpdate
import LitText.Process
--    .OneConstruct
--import LitText.Producer
--    .Servers ( dirQueriesRel)
import LitText.CmdLineUtilities
--    Process.UtilsParseArgs
--import Process.UtilsProcessing

programName = "construc6" :: Text
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
    inp <- parseAndStartExecute True "constructReport.txt"  t1 t2
--    let args = inArgs inp
    homeDirz <- homeDir2  -- used only to place the result file
    let resultFile = addFileName homeDirz ("resultFileConstruct6.txt" ::FilePath)
                             :: Path Abs File
--    let args = inArgs inp
    when (isZero . showT . inDestinationDir $ inp) $
            errorT ["parseAndExecuteNTmake: process construc6 - "
                , "requires base (dataset)"]
--    when (isNothing . inGraph $ inp) $
--            errorT ["parseAndExecuteNTmake: process construc6 - requires graph "]
    when (isZero . showT . inOriginDir $ inp) $
            errorT ["parseAndExecuteNTmake: process construc6 "
                , "- requires origin (source) dir "]
    case inFilename inp of
        Nothing -> do
                putIOwords ["parseAndExecute: process all constructs"]
                res <- processAll (oneConstruct2 inp)
                        isConstruct (inOriginDir inp) (inResultFile inp)
                putIOwords ["parseAndExecute: process all constructs result", res]
                return ()
        Just fn -> do
--                let queryFile = getFn args
--                    timeout = getTimeout args
--                    graph = Just . s2t $ argGraph args
--                    dbarg = (s2t $ argDB args)  :: Text
                oneConstruct2 inp
                        (addFileName (inOriginDir inp) fn)
                        -- open question whether the automatic
                        -- construction of result file name is ok
                return ()


isConstruct :: Path Abs File -> Bool
isConstruct  = hasExtension (Extension "construct")
-- todo include in typedfiles - hasType ...
