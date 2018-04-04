 -----------------------------------------------------------------------------
--
-- Module      :  oneFile
-- Copyright   :  andrew u frank -
--
-- | store the triples in an nt file in the fuseki triple store
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables
    , RecordWildCards  #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}


module Process.StoreOneFile (module Process.StoreOneFile
    , URI, serverBrest, serverLocalhost
    ) where

import           Test.Framework
import           Uniform.FileIO hiding ((<>), (</>), (<.>))
--import           Uniform.Filenames (homeDir)
import           Uniform.Strings
import           Uniform.Error

import CmdLineUtilities.UtilsProcessCmd
--import CmdLineUtilities.UtilsProcessCmd (LitTextFlag(..))
import CmdLineUtilities.UtilsProcessing

--import Uniform.HttpGet (makeHttpGet7, addPort2URI)
import Uniform.HttpCall (callHTTP10post
                , addPort2URI, URI, HttpVarParams(..)
                , makeAbsURI)
import Data.RDFext.Extension (ntFileTriples, sparqlConstructFile, turtleFile)
import LitTypes.TextDescriptor (serverBrest, serverLocalhost
            , rdfBase, IRI,
            , LitTextFlags(..), getServer)
import Data.List.Split (chunksOf)
import Data.Either (isRight)

import qualified Data.ByteString as BS
import qualified Codec.Compression.GZip as GZip
--import Uniform.StringInfix ((<>))
import Uniform.HttpCall (addToURI)
import Uniform.Convenience.ReadCSV

ntExtension = Extension "nt"
ntgzExtension = Extension "nt.gz"
flagExtension a = Extension . t2s $ ("fusekiFlag_" <> a)

putFilesFromCVS3 :: Inputs -> Path Abs File -> ErrIO Text
-- read the booknr from cvs file and store
putFilesFromCVS3 inp@Inputs{..} csvFileName = do
        csvfile <- readCSV csvFileName
        putIOwords ["putFilesFromCVS csv file is", showT csvfile]
        let header = headNote "sdwer43453" csvfile
        putIOwords ["putFilesFromCVS header", showT header]
        let csvfile2 = reverse . tail . reverse . tail $ csvfile
        -- to get rid of the empty trailing and the header line
        when (isDebugFlag inp) $ putIOwords
                ["putFilesFromCVS content csvfile2", showT csvfile2]

        let booknrs = map s2t . map (!!0) $ csvfile2
                -- the booknr are in the first postiion
        putIOwords ["putFilesFromCVS filenames to fetch", showT booknrs]

        let ntfilenames = map (convertBookNr2ntFilename inOriginDir) booknrs

        res <- mapM (putOneFile4 inp) ntfilenames

        putIOwords ["processCSVfile done with result ", showT . take 100 $ res]
--        writeFile2 (inResultFile) (concat' res)
        return . concat' $ res

convertBookNr2ntFilename :: Path Abs Dir -> Text -> Path Abs File
-- convert a single booknr to the correspoinding NT file name
convertBookNr2ntFilename originDir bn =
                addFileName  originDir  fn :: Path Abs File
--                  (addFileName  homeDir  (t2s bn ::FilePath))
    where fn = addExtension  ntgzExtension
                $  makeRelFile (t2s bn ::FilePath) :: Path Rel File


putOneFile4 :: Inputs -> Path Abs File -> ErrIO Text
-- put one file into the db and graph
-- checks for the flag to avoid duplication
-- but does separately check (and possibly duplicate) an nt and a nt.gz file

putOneFile4 inp@Inputs{..} fn = do
    putIOwords ["putOneFile4 x", "file", showT fn ]
    -- check for flagexisting
    let fnFlag = addExtension
            (flagExtension (maybe "default" id inGraph)) fn
                :: Path Abs File
    flagExist :: Bool <- doesFileExist' fnFlag
    flagTime :: EpochTime <- if flagExist
                                then getFileModificationTime fnFlag
                                else return 0
    ntTime :: EpochTime <- getFileModificationTime fn

    when (isDebugFlag inp) $ putIOwords ["putOneFile4 db - flag"
                    ,  showT fnFlag, "exist", showT flagExist]
    if not (isForceFlag inp) &&  (ntTime < flagTime)
            then return . unwords' $ ["putOneFile4 - nothing to do", showT fn]
            else do
                putIOwords ["putOneFile4 file to process - call putOneFile5", showT fn ]
                resp <- putOneFile5 inp fn
                writeFile2 fnFlag resp
                return resp

putOneFile5 :: Inputs -> Path Abs File
    ->  ErrIO Text
-- put one file into the db and graph
putOneFile5 inp@Inputs{..} fn = do
--        let fn = fromJustNote "putOneFile5 - filename is empty" $ fn
        when (isDebugFlag inp) $ putIOwords ["putOneFile5 db - graph", showT inDB
                , showT  inGraph  , "\nfile", showT fn]
        let ext0 =  unExtension . getExtension $  fn :: String
        when (isDebugFlag inp)  $ putIOwords ["putOneFile5 db - extension",  s2t ext0]
        trips :: LazyByteString<- case ext0 of
            "" -> do
                putIOwords ["putOneFile5", "extension is null"]
                error "putOneFile5 extension is null"
            "nt" -> do
                putIOwords ["putOneFile5", "extension is nt"]
                readFile2 fn
            "ttl" -> do
                putIOwords ["putOneFile5", "extension is ttl"]
                readFile2 fn
            "gz" -> do
                putIOwords ["putOneFile5", "extension is gz"]
                gz <- readFile2 fn   --GZip.compress . b2bl . t2b . unlines'
                let unzipped = GZip.decompress gz
                return unzipped

        let pathName = inDB  -- </> "update"
        let mgraph2 = fmap (\p -> PartURI $ (unPartURI rdfBase) </> p) inGraph
--        let mgraph2 = makeAbsURI rdfBase  (unPartURI inGraph)
        when (isDebugFlag inp)  $ putIOwords ["putOneFile5 db - path"
                ,  showT pathName, "mgraph2", showT mgraph2]

        let fusekiServer = getServer inp
        when (isDebugFlag inp)  $ putIOwords ["putOneFile5 db - path"
                ,  showT pathName, "mgraph2", showT mgraph2]
--        errorT ["sfda"]
        resp <- post2store False -- (DebugFlag `elem` inFlags)
                        "text/turtle"
                        fusekiServer pathName mgraph2   trips zero Nothing


        when True  $ putIOwords ["putOneFile5 response\n",  resp, "for", showT fn]
        -- write the response into the flag file
        let resp2 = "putOneFile5 ok " <> (showT fn) <> resp
        return resp2
    `catchError` \e -> do
        -- problem is the timeout
        putIOwords ["\n\n ERROR: putOneFile5  error caught"
                , "\n typically timeout of server - start process again"
                , "error is not raised again"]
        putIOwords ["putOneFile5 arguments were db - graph"
                , showT inDB , showT inGraph  , "file"
                , showT inFilename , "\n\n"]
        return . unwords' $ ["putOneFile5 return after error", showT e]


------------------------------------------

db1 = "corpus28"
--graph1 = Just "g" -- http://server/unset-base/
graph1 = Just "t1" -- Just "http://gerastree.at/h" -- http://server/unset-base/
fn1 = makeAbsFile "/home/frank/additionalSpace/DataBig/LitTest/aesop/short.nt"

--test_11 = do
--    r <- runErr $ putOneFile2 True True serverBrest db1 graph1 fn1
--    assertBool (isRight r)

--- alt - splitting for update query
--(<!>) :: LazyByteString -> LazyByteString -> LazyByteString
--(<!>) = append'
--
--insertData :: Maybe Text -> LazyByteString -> LazyByteString
---- make an insert data update query with a graph name or default
--insertData mgraph trips = insertDataText <!> graphText  <!>  trips <!>  closeText
--    where
--        insertDataText, graphText, closeText :: LazyByteString
--        insertDataText = b2bl. t2b $ "INSERT DATA {"
--        graphText  = b2bl . t2b $ maybe "{"
--                    (\a ->  " GRAPH <" <> (showT rdfBase </> a) <> "> {")
--                    mgraph
--
--        closeText =b2bl. t2b $  "} }"

--oneSplit_ ::  Bool -> Text -> Text -> Maybe Text -> Text -> LazyByteString  ->  ErrIO Text
---- the fn is only for illustratin when processing to see progress
--oneSplit_ debug fusekiServer pathName mgraph fn split = do
--    -- form the instert query
--    let query1 = insertData mgraph split
--    when debug $ putIOwords ["oneSplit_ for",  fn]
----    when debug $ putIOwords ["oneSplit_ for",  fn]
--
--    res <- callHTTP9post True "application/sparql-update"
--                fusekiServer pathName
--                query1
--    when True $ putIOwords ["oneSplit_ done", fn]
--    return res


