-----------------------------------------------------------------------------
--
-- Module      :  CheckServers
-- Copyright   :  andrew u frank -
--
-- |  check whether the servers are working
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}


module Processor.CheckServers
    (module Processor.CheckServers
    ) where

import           Test.Framework

import Uniform.HttpGet
import Parser.Foundation
import Producer.Servers
--import Main2sub
---- import CoreNLP.Snippets2nt as Snippets2nt (nlp_serverLoc, host_serverLoc)
--
--import qualified Pipes as Pipe
--import qualified Pipes.Prelude as Pipe
--import Pipes ((>->), (~>))
---- todo fileio - export for pipes
--
--
--import Uniform.Error
--import Uniform.FileIO
--import Uniform.FileStatus
----import Uniform.Strings hiding ((</>),(<|>))


checkAll :: ErrIO Text
-- check to assure that the servers work
-- return should be null
-- or a list of error msg
checkAll = do
        e <- check9000english
        g <- check9001german
        tt <- check17701tt
        return (concat' [e,g,tt])

test_checkAll = do
    r <-  (runErr checkAll)
    assertEqual (Right "") r

check9000english :: ErrIO Text
-- ^ test the NLP server for english
check9000english = do
    response <- makeHttpPost7 True
            destTest9000
            varsTest9000
            mimetypeTest9000e
            bodyTest9000e
    putIOwords ["response from 9000 english brest \n",showT response]
    if response == okResponse9000e then return "" else return response

destTest9000 = addPort2URI  serverBrest 9000
varsTest9000 = [("annotators", "tokenize,pos")]
mimetypeTest9000e = "test/application"
bodyTest9000e = "This is."
okResponse9000e = "{\"sentences\":[{\"index\":0,\"tokens\":[{\"index\":1,\"word\":\"This\",\"originalText\":\"This\",\"characterOffsetBegin\":0,\"characterOffsetEnd\":4,\"pos\":\"DT\",\"before\":\"\",\"after\":\" \"},{\"index\":2,\"word\":\"is\",\"originalText\":\"is\",\"characterOffsetBegin\":5,\"characterOffsetEnd\":7,\"pos\":\"VBZ\",\"before\":\" \",\"after\":\"\"},{\"index\":3,\"word\":\".\",\"originalText\":\".\",\"characterOffsetBegin\":7,\"characterOffsetEnd\":8,\"pos\":\".\",\"before\":\"\",\"after\":\"\"}]}]}"

check9001german :: ErrIO Text
-- ^ test the NLP server for german
check9001german = do
    response <- makeHttpPost7 True
            destTest9001
            varsTest9001
            mimetypeTest9001e
            bodyTest9001e
    putIOwords ["response from 9001 english brest \n",showT response]
    if response == okResponse9001e then return "" else return response

destTest9001 = addPort2URI  serverBrest 9001
varsTest9001 = [("annotators", "tokenize,pos")]
mimetypeTest9001e = "test/application"
bodyTest9001e = "Das ist so."
okResponse9001e = "{\"sentences\":[{\"index\":0,\"tokens\":[{\"index\":1,\"word\":\"Das\",\"originalText\":\"Das\",\"characterOffsetBegin\":0,\"characterOffsetEnd\":3,\"pos\":\"PDS\",\"before\":\"\",\"after\":\" \"},{\"index\":2,\"word\":\"ist\",\"originalText\":\"ist\",\"characterOffsetBegin\":4,\"characterOffsetEnd\":7,\"pos\":\"VAFIN\",\"before\":\" \",\"after\":\" \"},{\"index\":3,\"word\":\"so\",\"originalText\":\"so\",\"characterOffsetBegin\":8,\"characterOffsetEnd\":10,\"pos\":\"ADV\",\"before\":\" \",\"after\":\"\"},{\"index\":4,\"word\":\".\",\"originalText\":\".\",\"characterOffsetBegin\":10,\"characterOffsetEnd\":11,\"pos\":\"$.\",\"before\":\"\",\"after\":\"\"}]}]}"


check17701tt :: ErrIO Text
-- ^ check the treetagger for the POS for german
check17701tt = do
    response <- makeHttpPost7 True
            destTest17701
            varsTest17701
            mimetypeTest17701e
            bodyTest17701e
    putIOwords ["response from 17701 english brest \n",showT response]
    if response == okResponse17701e then return "" else return response

destTest17701 = addPort2URI  serverBrest 17701
varsTest17701 = []
mimetypeTest17701e = "test/application"
bodyTest17701e = "Das\nist\nso\n."
okResponse17701e = "Das\tPDS\tdie\nist\tVAFIN\tsein\nso\tADV\tso\n.\t$.\t.\n"
