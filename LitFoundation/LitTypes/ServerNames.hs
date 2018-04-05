-----------------------------------------------------------------------------
--
-- Module      :  Foundation - servers
-- Copyright   :  andrew u frank -
--
-- | the addresses of the servers
-----------------------------------------------------------------------------
--{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LitTypes.ServerNames (
    module LitTypes.ServerNames
   , module Uniform.Http 
    , IRI, mkIRI
    , ServerURI 
    , PartURI (..), unPartURI
    -- , URI, makeURI, makeAbsURI, parseAbsoluteURI
    -- , addPort2URI, addToURI, HttpVarParams (..)
    ) where


import Uniform.FileIO (makeAbsDir, makeRelDir, Text)
-- import Uniform.HttpURI -- hiding ((</>), (<.>))
import Data.RDFext.Extension (IRI, mkIRI)  -- (PartURI (..), unPartURI)
import Uniform.Http 

---- an attempt to have a read for URI  ReadS
--instance Read URI where
--    readsPrec _  = readS'
--
--readS' :: String -> [(URI, String)]
--readS' uri = [(fromJustNote ("read uri failed " ++ uri) . parseURI $ uri,"")]
----type PartURI = Text   -- should be defined in uniform.http?? todo
--        -- is defined in RDF.Extension


serverLocalhost, serverBrest :: ServerURI

serverLocalhost = localhost
serverBrest = mkServerURI "http://nlp.gerastree.at"

localhost = mkServerURI "http://127.0.0.1"

rdfBase, vocabularyBase :: IRI  -- not a real URI
rdfBase = IRI  "http://gerastree.at"
-- ^ for the text

vocabularyBase = IRI "http://gerastree.at"
-- for the vocabularies

-- dirQueries = makeAbsDir "/home/frank/additionalSpace/DataBig/Queries"
-- dirQueriesRel = makeRelDir "additionalSpace/DataBig/Queries"

-- ntDirsRel = makeRelDir "NT"  -- where it is for testing (can be expanded

treeTaggerPort = 9010 :: Int -- current range 9000 - 9010  -- is Wai warp port = Int

nlp = "nlp"::Text
--nlpURItext =  ( vocabularyBase) </> "nlp_2015" :: PartURI
nlpIRItext = append2IRI vocabularyBase  "nlp_2017" :: IRI
-- the 2017 vocabulary represents the dependency codes as properties (written lower case)
nlpUDEPtext = append2IRI vocabularyBase  "udep" :: IRI
-- the udep namespace is used only for the universal dependencies
-- not version specific but could be added
