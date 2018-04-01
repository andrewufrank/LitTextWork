-----------------------------------------------------------------------------
--
-- Module      :  Data.RDF.FileTypes
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :  af
-- Stability   :
-- Portability :
--
-- | the definition of filetypes for turtle (ttl) and ntriples (nt)
-- but when the triples as text, then read file with readFile2
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE TypeFamilies
    , DeriveGeneric
    , DeriveAnyClass  #-}
--{-# Option -w #-}

module Data.RDFext.FileTypes (
   module Data.RDFext.FileTypes
   , module Uniform.FileIO
   , Triple
  ) where

import qualified Data.RDF        as RDF
import Data.RDFext.Triple2text (triple2text, Triple)
import qualified          System.IO as S
import           Uniform.FileIO

import qualified Codec.Compression.GZip as GZip
import qualified Data.Text.IO           as TIO (hGetLine, hPutStr)



data RDFgraph =  RDFgraph (RDF.RDF RDF.TList)
    deriving (Show)

unRDFgraph (RDFgraph tlist) = tlist

--rdfGraphDebug = False  -- is set in TypedFile (where the class is defined)

--ntFile = makeTyped (Extension "nt") :: TypedFile5 RDFgraph ()
-- result is nt, not turtle!

ntFileTriples = makeTyped (Extension "nt") :: TypedFile5 [RDF.Triple] ()
data GZip  -- just a type, no data
ntFileTriplesGZip = makeTyped (Extension "nt.gz") :: TypedFile5 [RDF.Triple] GZip

data Turtle
turtleFile = makeTyped (Extension "ttl") :: TypedFile5 Text Turtle

data Queries
data Construct
-- just a definion, no content
sparqlQueryFile = makeTyped (Extension "query") :: TypedFile5 [Text] Queries
sparqlConstructFile = makeTyped (Extension "construct") :: TypedFile5 [Text] Construct

data Updates
-- just a definion, no content
sparqlUpdateFile = makeTyped (Extension "update") :: TypedFile5 [Text] Updates

rdfNTwrite hand nt = callIO $ RDF.hWriteRdf RDF.NTriplesSerializer hand nt


instance TypedFiles5 [RDF.Triple] GZip where
-- ^ files with full triples
--    mkTypedFile5 = TypedFile5 {tpext5 = Extension "nt.gz"}

--    typedExtension tp = tpext5 tp
--    isTyped :: Path Abs File -> TypedFile5 a b -> Bool
--    isTyped fp tp = (getExtension fp) == typedExtension tp
    append6 fp  tp triples = do

        when rdfGraphDebug $ putIOwords ["triples append6", showT fp]
        let fn2 = setExtension (tpext5 tp)  fp

        appendFile2 fn2 (GZip.compress . b2bl . t2b . unlines' $ Prelude.map triple2text triples)


    openHandle6 fp  tp = do
        when rdfGraphDebug $ putIOwords ["openHandle6 triples"]
        let ext = unExtension (tpext5 tp)
        let tmpext = Extension (ext <.> "tmp")
        let fn2 = setExtension tmpext  fp
        when rdfGraphDebug $ putIOwords ["openHandle6 triples", showT fn2]

        hand <- openFile2handle fn2 WriteMode
        -- should create or truncate the file, but not when the dir not exist
        --https://hackage.haskell.org/package/base-4.10.0.0/docs/System-IO.html#g:5
        when rdfGraphDebug $ putIOwords ["openHandle6 triples", showT fn2]
        return hand

    closeHandle6  fp tp hand = do
--        when rdfGraphDebug $
        putIOwords ["closeHandle6 triples"]
        let ext = unExtension (tpext5 tp)
        let tmpext = Extension (ext <.> "tmp")
        closeFile2 hand
        let fn2 = setExtension tmpext  fp
        let fn1 = setExtension (tpext5 tp) fp
        renameFile' fn2 fn1
--        when rdfGraphDebug $
        putIOwords ["closeHandle6 triples", showT fn2]
        return ()


    writeHandle6 hand  tp triples = do
--        when rdfGraphDebug $
        putIOwords ["writeHandle6 triples gz"]
        r <- write2handle  hand (GZip.compress . b2bl . t2b . unlines' $ Prelude.map triple2text triples)
--        when rdfGraphDebug $
        putIOwords ["writeHandle6 gz triples done "
                    , showT r ]
        return r

--    exist6 fp tp = do
--        let fn2 =  setExtension (tpext5 tp)  fp :: Path Abs File
--        doesFileExist'  fn2

    read6 fp  tp = error "read for triples is not easy and not required"


instance FileHandles [RDF.Triple] where
    write2handle h c = callIO $ TIO.hPutStr h (unlines' $ Prelude.map triple2text c)
    readLine4handle h = error "readLine4handle for RDF.Triple not implemented"
--        do
--            res <-  callIO $ TIO.hGetLine h
--            return . lines' $ res


instance TypedFiles5 [RDF.Triple] ()  where
-- ^ files with full triples
--    mkTypedFile5 = TypedFile5 {tpext5 = Extension "nt"}

    append6 fp  tp triples = do

        when rdfGraphDebug $ putIOwords ["triples append6", showT fp]
        let fn2 = setExtension (tpext5 tp)  fp

        appendFile2 fn2 (unlines' $ Prelude.map triple2text triples)

    openHandle6 fp  tp = do
--        when rdfGraphDebug $
        putIOwords ["openHandle6 triples"]
        let ext = unExtension (tpext5 tp)
        let tmpext = Extension (ext <.> "tmp")
        let fn2 = setExtension tmpext  fp
        when rdfGraphDebug $ putIOwords ["openHandle6 triples", showT fn2]

        hand <- openFile2handle fn2 WriteMode
        -- should create or truncate the file, but not when the dir not exist
        --https://hackage.haskell.org/package/base-4.10.0.0/docs/System-IO.html#g:5
--        when rdfGraphDebug $
        putIOwords ["openHandle6 triples", showT fn2]
        return hand

    closeHandle6  fp tp hand = do
--        when rdfGraphDebug $
        putIOwords ["closeHandle6 triples"]
        let ext = unExtension (tpext5 tp)
        let tmpext = Extension (ext <.> "tmp")
        closeFile2 hand
        let fn2 = setExtension tmpext  fp
        let fn1 = setExtension (tpext5 tp) fp
        renameFile' fn2 fn1
--        when rdfGraphDebug $
        putIOwords ["closeHandle6 triples", showT fn2]
        return ()


    writeHandle6 hand  tp triples = do
--        when rdfGraphDebug $
        putIOwords ["writeHandle6 triples no gz"]
        write2handle  hand (unlines' $ Prelude.map triple2text triples)  -- special !
        putIOwords ["writeHandle6 triples no gz done "]

--    exist6 fp tp = do
--        let fn2 =  setExtension (tpext5 tp)  fp :: Path Abs File
--        doesFileExist'  fn2

    read6 fp  tp = error "read for triples is not easy and not required"

instance TypedFiles5  Text  Turtle where
-- ^ files to contain turtle formated rdf
--    mkTypedFile5 = TypedFile5 {tpext5 = Extension "ttl"}
--      where e = mkExtension lpX "nt"

    append6 fp  tp queryText = error "not needed append6 for Turtle"

    read6 fp  tp = do
        let fn2 = setExtension (tpext5 tp)  fp
        raw :: Text <-  readFile2 fn2
        let res =  raw  -- changed for Text not []
        putIOwords ["read6  Turtle ",  res]  -- changed for Text not []
        return res

instance TypedFiles5 [Text] Queries where
-- ^ files with sparql queries
--    mkTypedFile5 = TypedFile5 {tpext5 = Extension "query"}
--      where e = mkExtension lpX "nt"

    append6 fp  tp queryText = error "not needed append6 for queries"

    read6 fp  tp = do
        let fn2 = setExtension (tpext5 tp)  fp
        raw :: Text <-  readFile2 fn2
        let res = lines' raw
        putIOwords ["read6  query ", unlines' res]
        return res

instance TypedFiles5 [Text] Construct where
-- ^ files with sparql queries
--    mkTypedFile5 = TypedFile5 {tpext5 = Extension "construct"}

    append6 fp  tp queryText = error "not needed append6 for Construct"

    read6 fp  tp = do
        let fn2 = setExtension (tpext5 tp)  fp
        raw :: Text <-  readFile2 fn2
        let res = lines' raw
        putIOwords ["read6  Construct ", unlines' res]
        return res


instance TypedFiles5 [Text] Updates where
-- ^ files with sparql queries
--    mkTypedFile5 = TypedFile5 {tpext5 = Extension "update"}
--      where e = mkExtension lpX "nt"

    append6 fp  tp queryText = error "not needed append6 for Updates"

    read6 fp  tp = do
        let fn2 = setExtension (tpext5 tp)  fp
        raw :: Text <-  readFile2 fn2
        let res = lines' raw
        when False $ putIOwords ["read6  Updates ", unlines' res]
        return res


data NTdescriptor = NTdescriptor {
-- ^ description of NT file
       destNT :: Path Abs File   -- the nt file
--     , destHandle :: Maybe Handle -- ^ the handle to write the nt triples to
     , gzipFlag :: Bool         -- ^ indicates whether the nt files should be gzip
    } deriving (Show, Read, Eq, Generic, Zeros)

openHandleTriples  :: NTdescriptor -> ErrIO  Handle
openHandleTriples textstate  = do
--    let mhand = destHandle textstate
--                putIOwords ["openHandleTriples", "to", showT $ destNT textstate]
                hand <- if gzipFlag textstate
                    then openHandle6 (destNT textstate) ntFileTriplesGZip
                    else openHandle6 (destNT textstate)  ntFileTriples
                return  ( hand)
--            `catchError` \e -> do
--                putIOwords ["openHandleTriples - error ", e , "file", showT $ destNT textstate]
----                openHandleTriples2 textstate
--                return textstate

--openHandleTriples2  :: NTdescriptor -> ErrIO NTdescriptor


writeHandleTriples :: NTdescriptor ->  Handle -> [Triple] -> ErrIO ()
writeHandleTriples  textstate hand tris = do
                putIOwords ["writeHandleTriples 2", showT textstate]
--                mhand2 <- openHandleTriples textstate mhand1
--                let hand = fromJustNote "writeHandleTriples" mhand2
                r <- if gzipFlag textstate
                    then writeHandle6 hand ntFileTriplesGZip tris
                    else writeHandle6 hand ntFileTriples tris
                putIOwords ["writeHandleTriples done", showT r]
                return ()

closeHandleTriples :: NTdescriptor ->   Handle -> ErrIO ()
closeHandleTriples textstate hand = do
--                let hand = fromJustNote "closeHandleTriples" mhand
                r <- if gzipFlag textstate
                    then closeHandle6 (destNT textstate) ntFileTriplesGZip hand
                    else closeHandle6 (destNT textstate) ntFileTriples hand
--                let textstate2 = textstate{destHandle=Nothing}
                putIOwords ["closeHandleTriples done", showT r, showT textstate]
                return r

