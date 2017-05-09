-----------------------------------------------------------------------------
--
-- Module      :  Store.RDFstore.Checks
-- Copyright   :
--
-- | functions to read file and check turtle syntax
--
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Store.RDFstore.Checks
--     (module Store.RDFstore.Checks
--            , B.ByteString
--            )
         where

import           Uniform.Strings
--import qualified Data.Text.IO as T
import           Uniform.FileIO
--import Uniform.Error
--import Data.String
--import EitherError.Top


--import qualified Data.Text.Encoding as E
import qualified Data.ByteString.Char8 as B
import qualified Data.RDF              as RDF
import qualified System.IO             as S
--import  Data.RDF as RDF (RDF, TList)
import           Data.Map              as Map (fromList)
--import Control.Monad (when)

debugCheckTurtle = False

x = "readFileBytestring" :: String
y = "satwett" :: Text


readFileBytestring :: LegalPathname -> ErrIO B.ByteString
readFileBytestring fp = do
    let x = "readFileBytestring" ::Text
    putIOwords [s2t "readFileBytestring", showT fp]
    cont <- callIO $ B.readFile (unL fp)

    return cont


type TriplesGraph =  (RDF.RDF RDF.TList)


checkTurtle :: Text -> ErrIO (Either RDF.ParseFailure TriplesGraph)
-- ^ check the turtle code in text
checkTurtle ttl = do
        putIOwords ["checkTurtle", showT baseurl]
        let res = RDF.parseString (  RDF.TurtleParser baseurl1 Nothing) ttl
        -- to catch errors - function is not total!
        putIOwords ["checkTurtle done", showT res]
        return res
    where
        baseurl1 =Just (RDF.BaseUrl . s2t $ "http://auf.us.to/doc") :: Maybe RDF.BaseUrl


checkTurtle2 :: Text -> [Text] -> ErrIO (Either RDF.ParseFailure TriplesGraph)
-- ^ convert a text with turtle code into tripleGraph
-- the baseURL (first param) is the code used for the : prefix
checkTurtle2 baseurlText ttl =  do
        let baseurl = Just (RDF.BaseUrl baseurlText) :: Maybe RDF.BaseUrl
        when debugCheckTurtle $ putIOwords ["checkTurtle", showT baseurl]
        let res = RDF.parseString (  RDF.TurtleParser baseurl Nothing) (unlines' ttl)
        -- to catch errors - function is not total!
        when debugCheckTurtle $ putIOwords ["checkTurtle done", showT res]
        return res

checkTurtle1 :: Text ->  Text -> Text
                    -> ErrIO (Either RDF.ParseFailure TriplesGraph)

-- ^ convert a text with turtle code into tripleGraph
-- the baseURL (first param) is the code used for the : prefix
-- the second text is the pm as turtle text
checkTurtle1 baseurlText pm ttl = do
        putIOwords [s2t "checkTurtle1 the code ", pm, ttl]
        let baseurl = Just (RDF.BaseUrl baseurlText) :: Maybe RDF.BaseUrl
        when debugCheckTurtle $ putIOwords  ["checkTurtle", showT baseurl]
        let res = RDF.parseString (  RDF.TurtleParser baseurl Nothing) (pm <> ttl)
        -- to catch errors - function is not total!
        when debugCheckTurtle $ putIOwords ["checkTurtle done", showT res]
        return res


emptyPM  = RDF.PrefixMappings $ Map.fromList []
baseurl =Just (s2t "http://auf.us.to/doc") :: Maybe Text

writeTurtle :: TriplesGraph -> FilePath -> ErrIO ()
writeTurtle gra fp = callIO $ do
    h <- S.openFile fp S.WriteMode
    putIOwords ["writeTurtle ", showT baseurl, showT emptyPM, "on", showT fp ]
    RDF.hWriteRdf (  RDF.TurtleSerializer baseurl emptyPM) h gra
    S.hClose h
    putIOwords ["writeTurtle done " ]
