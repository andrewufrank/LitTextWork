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
{-# LANGUAGE TypeFamilies  #-}
--{-# Option -w #-}

module Data.RDF.FileTypes (
   module Data.RDF.FileTypes
-- RDFgraph (..), unRDFgraph
-- , ntFile, ntFileTriples)
  ) where

import qualified Data.RDF        as RDF
import Data.RDF.Triple2text (triple2text)
import qualified          System.IO as S
import           Uniform.FileIO
import           Uniform.FileIO (EpochTime, getFileModificationTime)
--import Uniform.TypedFile (append6) -- should be included in fileio
import           Uniform.Strings hiding ((<.>), (</>))
--import Uniform.FilenamesAlgebra
import Uniform.FileStrings
--import Data.Text.IO (hPutStr)
-- todo fileio
import qualified Codec.Compression.GZip as GZip

--data TurtleData = TurtleData (RDF.RDF RDF.TList)
-- TODO - is this necessary? useful?

data RDFgraph =  RDFgraph (RDF.RDF RDF.TList)
    deriving (Show)

unRDFgraph (RDFgraph tlist) = tlist

rdfGraphDebug = False

--ntFile = mkTypedFile5 :: TypedFile5 RDFgraph ()
-- result is nt, not turtle!

ntFileTriples = mkTypedFile5 :: TypedFile5 [RDF.Triple] ()
ntFileTriplesGZip = mkTypedFile5 :: TypedFile5 [RDF.Triple] GZip
data Queries
-- just a definion, no content
sparqlQueryFile = mkTypedFile5 :: TypedFile5 [Text] Queries

data Updates
-- just a definion, no content
sparqlUpdateFile = mkTypedFile5 :: TypedFile5 [Text] Updates

rdfNTwrite hand nt = callIO $ RDF.hWriteRdf RDF.NTriplesSerializer hand nt

data GZip  -- just a type, no data

instance TypedFiles5 [RDF.Triple] GZip where
-- ^ files with full triples
    mkTypedFile5 = TypedFile5 {tpext5 = Extension "nt.gz"}

    append6 fp  tp triples = do

        when rdfGraphDebug $ putIOwords ["triples append6", showT fp]
        let fn2 = setExtension (tpext5 tp)  fp

        appendFile2 fn2 (GZip.compress . b2bl . t2b . unlines' $ Prelude.map triple2text triples)

    write6 fp  tp triples = do

        when rdfGraphDebug $ putIOwords ["triples write6", showT fp]
        let fn2 = setExtension (tpext5 tp)  fp
        when rdfGraphDebug $ putIOwords ["triples write6 fn", showT fn2]
        hand <- openFile2handle fn2 WriteMode
--        when rdfGraphDebug $ putIOwords ["triples write6", showT fn2]

        write2handle hand (GZip.compress . b2bl . t2b . unlines' $ Prelude.map triple2text triples)

--        when rdfGraphDebug $ putIOwords ["triples write6", showT fn2]
        closeFile2  hand
--        when rdfGraphDebug $ putIOwords ["triples write6", showT fn2]

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
        when rdfGraphDebug $ putIOwords ["closeHandle6 triples"]
        let ext = unExtension (tpext5 tp)
        let tmpext = Extension (ext <.> "tmp")
        closeFile2 hand
        let fn2 = setExtension tmpext  fp
        let fn1 = setExtension (tpext5 tp) fp
        renameFile fn2 fn1
        when rdfGraphDebug $ putIOwords ["closeHandle6 triples", showT fn2]
        return ()


    writeHandle6 hand  tp triples = do
        when rdfGraphDebug $ putIOwords ["writeHandle6 triples"]
        write2handle  hand (GZip.compress . b2bl . t2b . unlines' $ Prelude.map triple2text triples)

    exist6 fp tp = do
        let fn2 =  setExtension (tpext5 tp)  fp :: Path Abs File
        doesFileExist'  fn2

    read6 fp  tp = error "read for triples is not easy and not required"

    modificationTime6 fp tp = do
        let fn2 =  setExtension (tpext5 tp)  fp :: Path Abs File
        t :: EpochTime <- getFileModificationTime fn2
--        st <- getFileStatus fn2
--        let t = getModificationTimeFromStatus st
        return t

instance TypedFiles5 [RDF.Triple] ()  where
-- ^ files with full triples
    mkTypedFile5 = TypedFile5 {tpext5 = Extension "nt"}

    append6 fp  tp triples = do

        when rdfGraphDebug $ putIOwords ["triples append6", showT fp]
        let fn2 = setExtension (tpext5 tp)  fp

        appendFile2 fn2 (unlines' $ Prelude.map triple2text triples)

    write6 fp  tp triples = do

        when rdfGraphDebug $ putIOwords ["triples write6", showT fp]
        let fn2 = setExtension (tpext5 tp)  fp
        when rdfGraphDebug $ putIOwords ["triples write6 fn", showT fn2]
        hand <- openFile2handle fn2 WriteMode
--        when rdfGraphDebug $ putIOwords ["triples write6", showT fn2]

        write2handle  hand (unlines' $ Prelude.map triple2text triples)

--        when rdfGraphDebug $ putIOwords ["triples write6", showT fn2]
        closeFile2  hand
--        when rdfGraphDebug $ putIOwords ["triples write6", showT fn2]

    openHandle6 fp  tp = do
        when rdfGraphDebug $ putIOwords ["openHandle6 triples"]
        let ext = unExtension (tpext5 tp)
        let tmpext = Extension (ext <.> "tmp")
        let fn2 = setExtension tmpext  fp
        when True $ putIOwords ["openHandle6 triples", showT fn2]

        hand <- openFile2handle fn2 WriteMode
        -- should create or truncate the file, but not when the dir not exist
        --https://hackage.haskell.org/package/base-4.10.0.0/docs/System-IO.html#g:5
        when rdfGraphDebug $ putIOwords ["openHandle6 triples", showT fn2]
        return hand

    closeHandle6  fp tp hand = do
        when rdfGraphDebug $ putIOwords ["closeHandle6 triples"]
        let ext = unExtension (tpext5 tp)
        let tmpext = Extension (ext <.> "tmp")
        closeFile2 hand
        let fn2 = setExtension tmpext  fp
        let fn1 = setExtension (tpext5 tp) fp
        renameFile fn2 fn1
        when rdfGraphDebug $ putIOwords ["closeHandle6 triples", showT fn2]
        return ()


    writeHandle6 hand  tp triples = do
        when rdfGraphDebug $ putIOwords ["writeHandle6 triples"]
        write2handle  hand (unlines' $ Prelude.map triple2text triples)

    exist6 fp tp = do
        let fn2 =  setExtension (tpext5 tp)  fp :: Path Abs File
        doesFileExist'  fn2

    read6 fp  tp = error "read for triples is not easy and not required"

instance TypedFiles5 [Text] Queries where
-- ^ files with sparql queries
    mkTypedFile5 = TypedFile5 {tpext5 = Extension "query"}
--      where e = mkExtension lpX "nt"

    append6 fp  tp queryText = error "not needed append6 for queries"

    write6 fp  tp queryText = do

        when rdfGraphDebug $ putIOwords ["sparql queries write6", showT fp]
--        let fn2 = fp </> addExt lpX fn (tpext tp)  -- :: LegalPathname
        let fn2 = setExtension (tpext5 tp)  fp
        when rdfGraphDebug $ putIOwords ["sparql queries write6 fn", showT fn2]
        hand <- openFile2handle fn2 WriteMode
--        when rdfGraphDebug $ putIOwords ["sparql queries write6", showT fn2]

        write2handle  hand   (unlines' queryText)

--        when rdfGraphDebug $ putIOwords ["sparql queries write6", showT fn2]
        closeFile2  hand
--        when rdfGraphDebug $ putIOwords ["sparql queries write6", showT fn2]

    exist6 fp tp = do
        let fn2 =  setExtension (tpext5 tp)  fp :: Path Abs File
        doesFileExist'  fn2

    read6 fp  tp = do
        let fn2 = setExtension (tpext5 tp)  fp
        raw :: Text <-  readFile2 fn2
        let res = lines' raw
        putIOwords ["read6  query ", unlines' res]
        return res

instance TypedFiles5 [Text] Updates where
-- ^ files with sparql queries
    mkTypedFile5 = TypedFile5 {tpext5 = Extension "update"}
--      where e = mkExtension lpX "nt"

    append6 fp  tp queryText = error "not needed append6 for Updates"

    write6 fp  tp queryText = do

        when rdfGraphDebug $ putIOwords ["sparql Updates write6", showT fp]
--        let fn2 = fp </> addExt lpX fn (tpext tp)  -- :: LegalPathname
        let fn2 = setExtension (tpext5 tp)  fp
        when rdfGraphDebug $ putIOwords ["sparql Updates write6 fn", showT fn2]
        hand <- openFile2handle fn2 WriteMode
--        when rdfGraphDebug $ putIOwords ["sparql Updates write6", showT fn2]

        write2handle  hand   (unlines' queryText)

--        when rdfGraphDebug $ putIOwords ["sparql Updates write6", showT fn2]
        closeFile2  hand
--        when rdfGraphDebug $ putIOwords ["sparql Updates write6", showT fn2]

    exist6 fp tp = do
        let fn2 =  setExtension (tpext5 tp)  fp :: Path Abs File
        doesFileExist'  fn2

    read6 fp  tp = do
        let fn2 = setExtension (tpext5 tp)  fp
        raw :: Text <-  readFile2 fn2
        let res = lines' raw
        when False $ putIOwords ["read6  Updates ", unlines' res]
        return res

--instance TypedFiles5 RDFgraph () where
---- ^ files with full triles
--    mkTypedFile5 = TypedFile5 {tpext5 = Extension "nt"}
----      where e = mkExtension lpX "nt"
--
--    write5 fp fn tp (RDFgraph nt) = do
--        when rdfGraphDebug $ putIOwords ["RDFgraph write4", showT fp, showT fn]
----        let fn2 = fp </> addExt lpX fn (tpext tp)  -- :: LegalPathname
--        let fn2 = fp </> (fn <.> tpext5 tp) -- :: LegalPathname
--        when rdfGraphDebug $ putIOwords ["RDFgraph write4 effective fn", showT fn2]
--        hand <- openFile2handle fn2 WriteMode
--        when rdfGraphDebug $ putIOwords ["RDFgraph opened", showT fn2]
--        rdfNTwrite hand nt
--        when rdfGraphDebug $ putIOwords ["RDFgraph written", showT fn2]
--        closeFile2  hand
--        when rdfGraphDebug $ putIOwords ["RDFgraph closed", showT fn2]
--
--    read5 fp fn tp = do
--        let fn2 = fp </> (fn <.> tpext5 tp) -- :: LegalPathname
----        let fn2 = fp </> addExt lpX fn (tpext tp)  -- :: LegalPathname
--        raw <- readFile2 fn2
--        let ent = RDF.parseString RDF.NTriplesParser raw
--        case ent of
--            Left msg -> throwErrorT ["read4 ntdata", showT msg]
--            Right nt -> return (RDFgraph nt)
