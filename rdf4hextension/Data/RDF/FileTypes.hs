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
--
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}
--{-# Option -w #-}

module Data.RDF.FileTypes (RDFgraph (..), unRDFgraph, ntFile) where

import qualified Data.RDF        as RDF
import qualified          System.IO as S
import           Uniform.FileIO
import           Uniform.Strings hiding ((<.>), (</>))
import Uniform.FilenamesAlgebra
--import Uniform.FileIO (LegalPathname)


--data TurtleData = TurtleData (RDF.RDF RDF.TList)
-- TODO - is this necessary? useful?

data RDFgraph =  RDFgraph (RDF.RDF RDF.TList)
    deriving (Show)

unRDFgraph (RDFgraph tlist) = tlist

rdfGraphDebug = False

ntFile = mkTypedFile :: TypedFile RDFgraph
-- result is nt, not turtle!

rdfNTwrite hand nt = callIO $ RDF.hWriteRdf RDF.NTriplesSerializer hand nt

instance TypedFiles RDFgraph where
-- ^ files with full triles
    mkTypedFile = TypedFile {tpext = e}
      where e = mkExtension lpX "nt"

    write4 fp fn tp (RDFgraph nt) = do
        when rdfGraphDebug $ putIOwords ["RDFgraph write4", showT fp, showT fn]
        let fn2 = fp </> addExt lpX fn (tpext tp)  -- :: LegalPathname
        when rdfGraphDebug $ putIOwords ["RDFgraph write4 effective fn", showT fn2]
        hand <- openFile fn2 WriteMode
        when rdfGraphDebug $ putIOwords ["RDFgraph opened", showT fn2]
        rdfNTwrite hand nt
        when rdfGraphDebug $ putIOwords ["RDFgraph written", showT fn2]
        closeFile2 hand
        when rdfGraphDebug $ putIOwords ["RDFgraph closed", showT fn2]

    read4 fp fn tp = do
        let fn2 = fp </> addExt lpX fn (tpext tp)  -- :: LegalPathname
        raw <- readFile2 fn2
        let ent = RDF.parseString RDF.NTriplesParser raw
        case ent of
            Left msg -> throwErrorT ["read4 ntdata", showT msg]
            Right nt -> return (RDFgraph nt)
