-----------------------------------------------------------------------------
--
-- Module      :  Parser . Foundation
-- Copyright   :  andrew u frank -
--
-- | the definitions which are at the bottom
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parser.Foundation (
        module Parser.Foundation
    -- , module Data.RDF.Extension
    , module Uniform.Strings  -- cannot export FileIO as well
    , module Uniform.FileIO
    ) where

-- import           Data.RDF.Extension
import           Uniform.FileIO  -- (Path (..), Abs, Dir, File)
import           Uniform.Strings hiding ((</>), (<.>))   -- hiding ((<|>))
import System.IO (Handle)  -- todo include in FileIO exports

import Producer.Servers
import           Test.Framework

buchnameText = s2t . buchname
authorText = s2t . authorDir
originalsDir = sourceDir . source
serverLoc =  server . source

-- | the descriptor where the output should go
data DestDescriptor = OutFile {ddFile:: Path Abs File}
                    -- ^ the path is where the files should go
                    -- the filename is - if set the current Filename to append to
                    | OutHandle Handle
                    | TripleStoreGraph {ddURI:: URI, ddGraph :: Text}
                    | NotKnown
                    deriving (Show, Eq)

-- | the description of a file to operate as texts - make legalfilen, when needed
data TextState2 = TextState2
    {                -- the projp buchcode gives the code for the book,
                -- add the element number
     source :: TextSource
--     serverLoc       :: URI  -- where the nlp servers are
--    , originalsDir :: Path Abs Dir -- the directory in which the files are
--                    -- either LitOrig or a test dir
    , authorDir    :: FilePath -- ^ the directory where the inputs in the LitOriginal directory are
                        -- the project
                                 -- and where the converted data go
    , buchname     :: FilePath -- filename in directory gives the buch sigl
    , textfilename :: Path Abs File -- the input path of the file with the triples
    , tripleOutDesc :: DestDescriptor
                -- a description where the ouptut goes
    } deriving (Show, Eq)

-- | the descriptor where the output should go
data DestGenerality = DGoutDir {dgDir:: Path Abs Dir}
                    -- ^ the path is where the files should go
                    -- the filename is - if set the current Filename to append to
--                    | OutHandle Handle
                    | DGtripleStore {dgURI:: URI }
--                    | NotKnown
                    deriving (Show, Eq)


-- | the description of where the files are and where the result shuld go
-- before any particular text is opened
data TextSource = TextSource
    {      server       :: URI  -- where the nlp servers are
    ,sourceDir :: Path Abs Dir -- the directory in which the files are


     }                     deriving (Show, Eq)

litTestDir = makeAbsDir "/home/frank/additionalSpace/DataBig/LitTest"
sourceE1 = TextSource {server = serverBrest, sourceDir = litTestDir}
generalityE1 = DGoutDir litTestDir

fillTextState2 :: TextSource -> DestGenerality -> FilePath -> FilePath -> TextState2
-- construct at text state with authorDir and buchFilename as FilePath
fillTextState2 ts dg author buch = TextState2 {
    source = ts
    , authorDir = author
    , buchname = buch
    , textfilename = (sourceDir ts) </> (author </> buch)
    , tripleOutDesc =  fillDestination dg author buch True
    }

fillDestination :: DestGenerality -> FilePath -> FilePath -> Bool -> DestDescriptor
-- | build the description of the destination
-- either a file or a uri with grah
-- later add handle with switch - true for file append output
-- false for use a handle
fillDestination  (DGoutDir dir) author buch True = OutFile (dir </> (author </> buch))
fillDestination  t _ _ _ = errorT ["Foundation - fillDestination not defined for ", showT t]


test_fillTextState10 = assertEqual res10 res
    where
        res = fillTextState2 sourceE1 generalityE1 "may" "test"
res10 =  TextState2 {source = TextSource
                        {server = makeURI "http://nlp.gerastree.at",
                        sourceDir = makeAbsDir "/home/frank/additionalSpace/DataBig/LitTest/"},
                authorDir ="may",
                buchname = "test",
            textfilename = makeAbsFile "/home/frank/additionalSpace/DataBig/LitTest/may/test",
            tripleOutDesc = OutFile
                {ddFile = makeAbsFile "/home/frank/additionalSpace/DataBig/LitTest/may/test"}}


