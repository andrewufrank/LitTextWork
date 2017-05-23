---------------------------------------------------------------------------
--
-- Module      :  MainLit
-- Copyright   :  andrew u frank -
--
-- |  producing the lit and nlp triples
-- in the same module to make sure the same text is used
--------------------------------------- --------------------------------------

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Main2sub
import           Parser.Foundation
import           Lines2para.Lines2para
import           Parser.ProduceLit
import           Parser.ProduceNLP
import           Parser.ProduceNLPtriples
import           Uniform.Convenience.StartApp
import           Uniform.FileIO
import           Uniform.Strings
import Producer.Servers

import CoreNLP.Snippets2nt

main = do
    startProg "april 18 = ProduceLit" "produce the lit for: entries " main1 -- main2tt
--    putIOwords ["test1", showT testF1]
--    putIOwords ["functionality test1", showT $ test1 == test1result]

main1 = do
    let textstate = TextState2 {
              serverLoc = serverBrest
           , originalsDir = makeAbsDir "/home/frank/additionalSpace/DataBig/LitTest"
--            , authorDir=  "carol" -- "waterhouse"
--            , buchname =  "minimal"  -- "pg11"
--            , authorDir=  "Boccacio" --
            -- , authorDir = "tawada"
            -- , buchname =  "Auge" -- "bad" -- "etueden"
--            , authorDir = "waterhouse"
            , authorDir = "test"
           , buchname =   "t1"  -- test der durchgeht
--            , buchname =   "t2"  -- test fuer lange zeilen
            -- , buchname =   "t4"  -- test fuer parazeilen
--            , buchname =  "day12"
            -- , buchname =  "test1"
            -- , buchname =  "kuw"
            -- , buchname =  "test1"
        , textfilename = makeAbsFile ("/home/frank/additionalSpace/DataBig/LitTest/test/t1")
             }

    putIOwords ["ProduceLit for", showT textstate]
    let litDebugOnly = False
    mainLitAndNLPproduction litDebugOnly textstate
