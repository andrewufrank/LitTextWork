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
--TextLoc (..)
      Zeilen (..)
    , TextZeilen (..)
    , TextState2 (..)
    , parseMarkup
    , BuchToken (..), Unparser (..)
    , LanguageCode (..)
--    , formatParaID, unparaID -- ParaID
--    , formatLineID
    , combine2linesWithHyphenation
    , countLeerzeilen
    , checkSeitenzahlen, readSeitenzahl
    , Zeros (..)
    , RDFproperty (..), RDFtype (..)
    , ParaSigl (..), unParaSigl
    , module Data.RDF.Extension
--    , ParaID (..)  -- for testing
    , LegalPathname (..)
    ) where


import           BuchCode.BuchToken
import           BuchCode.MarkupText
import           Data.RDF.Extension
import           Uniform.FileIO      (LegalPathname (..))
import           Uniform.StringInfix
import           Uniform.Strings     hiding ((<|>))
import           Uniform.Zero
-- litURI = gerastreeURI </> "lit_2014" :: PartURI

newtype ParaSigl = ParaSigl RDFsubj  deriving (Show, Eq)
unParaSigl (ParaSigl a) = a




-- | the description of a file to operate as texts - make legalfilen, when needed
data TextState2 = TextState2
    {                -- the projp buchcode gives the code for the book,
                -- add the element number
    endpoint       :: PartURI
    , serverLoc       :: PartURI
    , originalsDir :: LegalPathname -- the directory in which the files are
                    -- either LitOrig or a test dir
    , authorDir    :: Text -- ^ the directory where the inputs in the LitOriginal directory are
                        -- the project
                                 -- and where the converted data go
    , buchname     :: Text -- filename in directory gives the buch sigl
    , graph        :: PartURI -- the graoh in which the triples go
    -- , buchcode  :: PartURI  -- a unique identifier for the book within the lit project
            -- where to put the "markup.txt" files for NLP processing
--    , texts :: [Text]  -- the accumulator for the texts
--    , language  :: LanguageCode
-- the language code is now with the snipet and set by buchcode
    } deriving (Show )

--class Zeilen z where  -- is now in BuchCode
