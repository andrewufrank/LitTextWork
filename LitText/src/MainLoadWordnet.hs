---------------------------------------------------------------------------
--
-- Module      :  MainLoadWordnet
-- Copyright   :  andrew u frank -
--
-- |  loading the triples from wn31.nt
-- seems to work (but run on the fueseki server not remote!)
-- simpler: upload with upload from fuseki browser!
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- module MainLoadWordnet (main ) where

-- import           Parser.Foundation        hiding ((</>))
-- import           Parser.LinesToParagrahs
-- import           Parser.ProduceLit
-- import           Parser.ProduceNLP
-- import           Parser.ProduceNLPtriples
import           Store.Fuseki
import           Uniform.FileIO
import           Uniform.Strings
import           Uniform.Convenience.StartApp
-- for fuseki store
import qualified Network.HTTP            as Net
import Store.RDFstore.HttpCall

-- debugNLP = False
-- debugLit = True -- False


main  = do
    startProg "load wordnet files"  "" main1

main1 = do
    let wnFilepath = mkFilepath lpX ("/home/frank/additionalSpace/DataBig/LitOriginals" </>
                        "wordnet/wn31.nt/data.ttl" :: Text)
                        -- is turtle format!
    wn31nt  :: Text <- readFile2 wnFilepath

    let  endpoint = "http://nlp.gerastree.at:3030/marchDB/update"

    putIOwords ["MainLoadWordnet: file read", showT . length . lines' $ wn31nt
        , "\nwill be stored in ", endpoint]

    -- let wn31short = unlines' . take 5 . lines' $ wn31nt

    response :: Text <- storeNT fusekiServer  endpoint wn31nt (Just "http://gerastree.at/wn31")
    -- graph must be fully determined IRI

    putIOwords ["npl: triples stored with fuseki in graph   "
            ,  response ]

    return ()

debugFuseki = True

-- in httpCall
bb2t :: ByteString -> Text
bb2t = fromJustNote "bytestring conversionm bb2t" . b2t

-- in Fuseki
textWG triplesText gr1 = "INSERT DATA { GRAPH "  <>  gr1 <>
                " {" <> triplesText <> "} }"
textNG triplesText = "INSERT DATA"  <>
                " {" <> triplesText <> "  }"

storeNT fusekiServer endpoint nttext  graph = do
        when debugFuseki $
                putIOwords ["insertGraphF with text ", "url",  endpoint, "\n\ttext",  nttext ]

        let text = maybe (textNG nttext)  (textWG nttext . wrapInSpitz) graph
        let request = makeHTTPrequest5  Net.POST  endpoint "application/sparql-update" nttext

    --    when debugFuseki $

        when debugFuseki $
                putIOwords ["insertGraphF before call HTTP5 - the uriEnc is: ", showT endpoint
                    -- , "\n\trequest", showT request
                    -- , "\n\trequestbody",  bb2t $ Net.rqBody request
                    ]

        response <- callHTTP5  request
        -- let response = "nothing done"

        when debugFuseki $ putIOwords ["insertGraphF in:", endpoint, "result is:", response]

        return response



-- storeTriplesFuseki textstate tris = insertTriplesIntoGraph fusekiServer (endpoint textstate)
--             tris  (Just (gerastreeURI </> graph textstate ))
