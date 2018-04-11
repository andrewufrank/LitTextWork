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

module LitText.Foundation.ServerNames (
    module LitText.Foundation.ServerNames
   , module Uniform.Http
    , IRI, mkIRI
    ) where


import Uniform.FileIO (makeAbsDir, makeRelDir, Text)
import Uniform.Http
import Data.RDFext

serverLocalhost, serverBrest :: ServerURI

serverLocalhost = localhost
serverBrest = mkServerURI "http://nlp.gerastree.at"

localhost = mkServerURI "http://127.0.0.1"

rdfBase, vocabularyBase :: IRI  -- not a real URI
rdfBase = mkIRI  "http://gerastree.at/"
-- ^ for the text

vocabularyBase = mkIRI "http://gerastree.at/"
-- for the vocabularies

-- dirQueries = makeAbsDir "/home/frank/additionalSpace/DataBig/Queries"
-- dirQueriesRel = makeRelDir "additionalSpace/DataBig/Queries"

-- ntDirsRel = makeRelDir "NT"  -- where it is for testing (can be expanded

treeTaggerPort = 9010 :: Int -- current range 9000 - 9010  -- is Wai warp port = Int

nlp = "nlp"::Text
--nlpURItext =  ( vocabularyBase) </> "nlp_2015" :: PartURI
nlpIRItext = append2IRI vocabularyBase  "nlp_2017#" :: IRI
-- the 2017 vocabulary represents the dependency codes as properties (written lower case)
nlpUDEPtext = append2IRI vocabularyBase  "udep#" :: IRI
-- the udep namespace is used only for the universal dependencies
-- not version specific but could be added
