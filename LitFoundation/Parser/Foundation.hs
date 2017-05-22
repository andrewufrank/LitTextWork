-----------------------------------------------------------------------------
--
-- Module      :  Parser . Foundation
-- Copyright   :  andrew u frank -
--
-- | the definitions which are at the bottom
-----------------------------------------------------------------------------

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
import           Uniform.FileIO (Path (..), Abs, Dir, File)
import           Uniform.Strings    -- hiding ((<|>))
import Producer.Servers

buchnameText = s2t . buchname
authorText = s2t . authorDir

-- | the description of a file to operate as texts - make legalfilen, when needed
data TextState2 = TextState2
    {                -- the projp buchcode gives the code for the book,
                -- add the element number
     serverLoc       :: URI  -- where the nlp servers are
    , originalsDir :: Path Abs Dir -- the directory in which the files are
                    -- either LitOrig or a test dir
    , authorDir    :: FilePath -- ^ the directory where the inputs in the LitOriginal directory are
                        -- the project
                                 -- and where the converted data go
    , buchname     :: FilePath -- filename in directory gives the buch sigl
    , textfilename :: Path Abs File -- the input path of the file with the triples
                -- same as input, but with nt extension
                -- where is exension removed? -- initially remove markup?
    } deriving (Show )

--class Zeilen z where  -- is now in BuchCode
