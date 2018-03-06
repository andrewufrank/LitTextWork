-----------------------------------------------------------------------------
--
-- Module      :  LitTypes . TextDescriptor
-- Copyright   :  andrew u frank -
--
-- | the definitions of the descrption of the text and related types (e.g. Language)
-----------------------------------------------------------------------------
--{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module LitTypes.TextDescriptor (
        module LitTypes.TextDescriptor
    , module Uniform.Strings  -- cannot export FileIO as well
    , module Uniform.FileIO
    , module LitTypes.LanguageTypedText
    , LanguageCode (..) -- from rdf4hextension
    , RDFtypes (..)
    , RDFproperties (..)
    , NTdescriptor (..)
--    , module Path   -- to export IsString
    ) where

-- import           Data.RDF.Extension
import           Uniform.FileIO  -- (Path (..), Abs, Dir, File)
import           Uniform.Strings hiding ((</>), (<.>))   -- hiding ((<|>))
import System.IO (Handle)  -- todo include in FileIO exports
import Uniform.HttpURI (URI, makeURI)
import Producer.Servers  (serverBrest)  -- for test
import Data.RDF.Extension (LanguageCode (..), RDFtypes(..), RDFproperties (..))
import Data.RDF.FileTypes
--import           Test.Framework
import BuchCode.BuchToken hiding ((</>), (<.>))
import LitTypes.LanguageTypedText
import Process.UtilsParseArgs (LitTextFlags (..), LitTextFlag (..) )

-- directories:
litOriginals = makeRelDir "LitOriginals"
litTests =    makeRelDir "LitTest"
litDir = makeAbsDir "/home/frank/additionalSpace/DataBig"
litOrigDir1 = litDir </>litOriginals
litTestDir1 = litDir </> litTests
ntDir = makeAbsDir "/home/frank/Scratch/NT"
litNTOrigDir1 = ntDir </> litOriginals
litNTTestDir1 = ntDir </> litTests

-- | the description of a file to operate as texts - make legalfilen, when needed
data TextDescriptor = TextDescriptor
    {                -- the projp buchcode gives the code for the book,
                -- add the element number
     sourceMarkup :: Path Abs File -- the markup file
     -- selects then the type of the ntfile (nt or nt.gz)
     , nlpServer :: URI -- ^ where the nlp server is
    , authorDir    :: Text -- ^ the directory where the inputs in the LitOriginal directory are
                        -- the project
                                 -- and where the converted data go
    , buchName     :: Text -- ^ filename in directory gives the buch sigl
    , includeText :: Bool -- ^ full text is included in triples
--                    (false - only the analysis, sentence can be reconstructed
--                      but not the remainder of the text)
    , txPosTagset :: Text
    , ntdescriptor :: NTdescriptor
    } deriving (Show, Read,  Eq)

instance Zeros TextDescriptor where
    zero = TextDescriptor zero zero zero zero zero zero zero
--instance IsString (Path Abs File)

instance Zeros (Path Abs File) where
    zero = makeAbsFile ""
instance Zeros URI where
    zero = makeURI ""
instance Zeros Bool where
    zero = False
instance Zeros NTdescriptor where
    zero = NTdescriptor zero zero

-- S N I P

-- | a single language piece of text with lanuage code, length and start para number
data Snip = Snip { tz3loc :: TextLoc
                        , tz3para :: ParaNum
                        , tz3snipnr ::  Int
                        , tz3text:: LCtext
                        , tz3textLength :: Int
                        , tz3posTag :: Text
--                        , tz3lang :: LanguageCode
                        }
            deriving (Read, Show, Eq )

instance Zeros Snip where zero = Snip zero zero zero zero zero zero

--instance (Zeros a) => Zeros (Maybe a) where zero = Nothing
-- todo algebra

-------------- definitinos of TZ2

data TextLoc = TextLoc {tlpage :: Maybe Text, tlline :: Int} deriving (Read, Show, Eq)
-- ^ the place of a line in the full text
-- for simplification, all counts are from the start of the text
-- not relative to the page or paragraph (can be computed, if desired)
-- page number (tlline) is text, to deal with III etc.
    -- removed paraid

instance Zeros TextLoc where zero = TextLoc Nothing zero

-- the type of the line
data TextType = Text0 | Zahl0 | Fussnote0 | Kurz0 | Para0
        -- | AllCaps0
        deriving (Show, Read, Eq )

data TextWithMarks = TextWithMarks {twm::Text, twmMarks::[(Int, Text)]}
        deriving (Show, Read, Eq )
-- the text is without markers, the markers are a list
-- of offset of the marker from start of line resp. previous marker
-- and the marker as text
-- the list is empty if none


-- the format accumulation all detail info to build the triples.
-- only tzpara and tzmarkup in final result
-- has no language field because language is encoded as markup
data TZ =
         TZtext {tzt:: TextType, tzloc :: TextLoc
                    , tztext:: TextWithMarks   -- is this appropriate here?
--                    , tzlang :: LanguageCode
                    }
--        | TZpara  {tzloc :: TextLoc, tztzs :: [TZ], tzlang :: LanguageCode
--                , tlpara :: ParaNum
--                , tzInPart :: ParaNum}
        | TZmarkup  {tzloc :: TextLoc
                        , tztext:: TextWithMarks
                        , tztok :: BuchToken
--                        , tzlang :: LanguageCode
--                        , tlpara :: ParaNum
--                        , tzInPart :: ParaNum
                        }
        | TZleer  {tzloc :: TextLoc}
        | TZneueSeite  {tzloc :: TextLoc}
        | TZignore {tzloc :: TextLoc, tztext:: TextWithMarks}
            deriving (Read, Show, Eq )

instance Zeros TZ where zero = TZleer zero

data TextWithMarks1 = TextWithMarks1 {twm1::LCtext, twmMarks1::[(Int, Text)]}
        deriving (Show, Read, Eq )
-- the text is without markers, the markers are a list
-- of offset of the marker from start of line resp. previous marker
-- and the marker as text
-- the list is empty if none

codeTextWM :: LanguageCode -> TextWithMarks -> TextWithMarks1
codeTextWM lc t = TextWithMarks1 {twm1=codeText lc (twm t), twmMarks1=twmMarks t}

codeTextWM1 :: LanguageCode -> TextWithMarks1 -> TextWithMarks1
codeTextWM1 lc t = TextWithMarks1 {twm1=codeText lc (getText $ twm1 t), twmMarks1=twmMarks1 t}

-- | the input text after language has been distributed
data TZ1 =
         TZtext1 {tzt1:: TextType
                    , tzloc1 :: TextLoc
                    , tztext1:: TextWithMarks1   -- is this appropriate here?
--                    , tzlang1 :: LanguageCode
                    }
--        | TZpara  {tzloc :: TextLoc, tztzs :: [TZ], tzlang :: LanguageCode
--                , tlpara :: ParaNum
--                , tzInPart :: ParaNum}
        | TZmarkup1  {tzloc1 :: TextLoc
                        , tztext1:: TextWithMarks1
                        , tztok1 :: BuchToken
--                        , tzlang1 :: LanguageCode
--                        , tlpara :: ParaNum
--                        , tzInPart :: ParaNum
                        }
        | TZleer1  {tzloc1 :: TextLoc}
        | TZneueSeite1  {tzloc1 :: TextLoc}
        | TZignore1 {tzloc1 :: TextLoc, tztext1:: TextWithMarks1}
            deriving (Read, Show, Eq )

-- instance Zeros TZ1 where zero = TZleer zero

copyTZtoTZ1 :: TZ -> TZ1
copyTZtoTZ1 (TZtext t l m) = TZtext1 t l (codeTextWM NoLanguage m)
copyTZtoTZ1 (TZmarkup l m t) = TZmarkup1 l (codeTextWM NoLanguage m) t
copyTZtoTZ1 (TZignore l m ) = TZignore1 l (codeTextWM NoLanguage m)
copyTZtoTZ1 (TZleer l  ) = TZleer1 l
copyTZtoTZ1 x = error ("copyTZtoTZ1 missing case: " ++ show x)

getLanguage3TZ1 :: TZ1 -> LanguageCode
getLanguage3TZ1 = getLanguageCode . twm1 . tztext1

newtype ParaNum = ParaNum Int deriving (Read, Show, Eq)
-- just to avoid confusions
unparaNum (ParaNum t) = t
instance Zeros ParaNum where zero =  ParaNum zero

-- the format accumulation all detail info to build the triples.
-- only tzpara and tzmarkup in final result
data TZ2 =
     TZ2para  {tz2loc :: TextLoc
                , tz2tzs :: [TZ1]
--                , tz2lang :: LanguageCode
                , tz2para :: ParaNum
                , tz2inPart :: ParaNum}
    | TZ2markup  {tz2loc :: TextLoc
                , tz2text:: TextWithMarks1
                    , tz2tok :: BuchToken
--                    , tz2lang :: LanguageCode
                    , tz2para :: ParaNum
                    , tz2inPart :: ParaNum
                    }
            deriving (Read, Show, Eq )

data LitDirs = LitDirs {
        source :: Path Abs Dir
        , dest :: Path Abs Dir
        }

dirsTest = LitDirs litTestDir1  litNTTestDir1
dirsOrig = LitDirs litOrigDir1  litNTOrigDir1

fillTextState4a :: Path Abs File -> URI -> Path Abs Dir -> Text -> Text -> LitTextFlags
                -> TextDescriptor
-- construct at text state for a gutenberg catalog markup file
-- output is gzip, text is not included
fillTextState4a file server ntdir authordir buchname flags = TextDescriptor {
        sourceMarkup = file
        , nlpServer = server
        , authorDir = authordir
        , buchName = buchname
        , includeText = IncludeTextFlag `elem` flags
        , txPosTagset =  ""
        , ntdescriptor = fillNTdescriptor ntdir filename True
        }
--        fillTextState3 litdirs server author buch
    where
            filename = getFileName file :: Path Rel File
--             = getImmediateParentDir fp
--            buch = getNakedFileName fp

fillNTdescriptor :: Path Abs Dir -> Path Rel File -> Bool -> NTdescriptor
fillNTdescriptor  ntdir filename gzip   = NTdescriptor {
          destNT = (ntdir </> filename) :: Path Abs File
        , gzipFlag = gzip
--        , destHandle =  Nothing
        }

fillTextState3a :: LitDirs -> URI -> FilePath -> FilePath -> Bool
                -> TextDescriptor

-- construct at text state with authorDir and buchFilename as FilePath
fillTextState3a litdirs server author buch includeText = TextDescriptor {
    sourceMarkup = (source litdirs) </> (author </> buch)
--    , destNT = (dest litdirs) </> (author </> buch)
--    , gzipFlag = False
--    , destHandle = Nothing
    , nlpServer = server
    , authorDir = s2t author
    , buchName = s2t buch
    , includeText =  includeText
    , txPosTagset = ""   -- take default
    , ntdescriptor = fillNTdescriptor (dest litdirs) author_buch False
    }
        where
            author_buch = makeRelFile (author </> buch) :: Path Rel File
--            author1 = author litdirs
--            buch1 = buch litdirs

--test_fillTextState11 = assertEqual res10 res
--    where
--        res = fillTextState3 dirsOrig serverBrest "may" "test"
--res10 = TextDescriptor {
--        sourceMarkup = makeAbsFile
--            "/home/frank/additionalSpace/DataBig/LitOriginals/may/test"
--        , destNT = makeAbsFile
--            "/home/frank/Scratch/NT/LitOriginals/may/test"
--        , nlpServer = serverBrest
--        , authorDir = "may"
--        , buchName = "test"
--        , destHandle = Nothing
--         , gzipFlag = False
--        , includeText = True
--       }
--
--test_fillTextState12 = assertEqual res11 res
--    where
--        res = fillTextState4 dirsTest serverBrest fp
--        fp = makeAbsFile
--             "/home/frank/additionalSpace/DataBig/LitOriginals/may/test"
--res11 = TextDescriptor {
--        sourceMarkup = makeAbsFile
--            "/home/frank/additionalSpace/DataBig/LitTest/may/test"
--        , destNT = makeAbsFile
--            "/home/frank/Scratch/NT/LitTest/may/test"
--        , nlpServer = serverBrest
--        , authorDir = "may"
--        , buchName = "test"
--        , destHandle = Nothing
--        , gzipFlag = False
--        , includeText = True
--        }

--TextState2 {
--    sourceMarkup = makeDirAbs ""
--    , destNT = ntDir </> (author </> buch)
--    , authorDir = author
--    , buchname = buch

--fillTextState2 :: TextSource -> DestGenerality -> FilePath -> FilePath -> TextState2
---- construct at text state with authorDir and buchFilename as FilePath
--fillTextState2 ts dg author buch = TextState2 {
--    source = ts
--    , authorDir = author
--    , buchname = buch
--    , textfilename = (sourceDir ts) </> (author </> buch)
--    , tripleOutDesc =  fillDestination dg author buch True
--    }

---- | the descriptor where the nt output should go
--data DestDescriptor = OutFile {ddFile:: Path Abs File}
--                    -- ^ the path is where the files should go
--                    -- the filename is - if set the current Filename to append to
--                    | OutHandle Handle
--                    | TripleStoreGraph {ddURI:: URI, ddGraph :: Text}
--                    | NotKnown
--                    deriving (Show, Eq)


---- | the descriptor where the output should go
--data DestGenerality = DGoutDir {dgDir:: Path Abs Dir}
--                    -- ^ the path is where the files should go
--                    -- the filename is - if set the current Filename to append to
----                    | OutHandle Handle
--                    | DGtripleStore {dgURI:: URI }
----                    | NotKnown
--                    deriving (Show, Eq)


-- | the description of where the files are and where the result shuld go
-- before any particular text is opened
--data TextSource = TextSource
--    {      server       :: URI  -- where the nlp servers are
--    ,sourceDir :: Path Abs Dir -- the directory in which the files are
--
--
--     }                     deriving (Show, Eq)
--buchnameText = s2t . buchname
--authorText = s2t . authorDir
--originalsDir = sourceDir . source
--serverLoc =  server . source


--fillTextState3 :: LitDirs -> URI -> FilePath -> FilePath
--                -> TextDescriptor
---- construct at text state with authorDir and buchFilename as FilePath
--fillTextState3 litdirs server author buch = TextDescriptor {
--    sourceMarkup = (source litdirs) </> (author </> buch)
--    , destNT = (dest litdirs) </> (author </> buch)
--    , gzipFlag = False
--    , destHandle = Nothing
--    , nlpServer = server
--    , authorDir = s2t author
--    , buchName = s2t buch
--    , includeText = True
--}
--fillTextState3a :: LitDirs -> URI -> FilePath -> FilePath -> Bool
--                -> TextDescriptor
---- construct at text state with authorDir and buchFilename as FilePath
--fillTextState3a litdirs server author buch includeText = TextDescriptor {
--    sourceMarkup = (source litdirs) </> (author </> buch)
--    , destNT = (dest litdirs) </> (author </> buch)
--    , gzipFlag = False
--    , destHandle = Nothing
--    , nlpServer = server
--    , authorDir = s2t author
--    , buchName = s2t buch
--    , includeText =  includeText
--    }

--authorName = s2t . authorDir
--buchName = s2t . buchName

--fillTextState4 :: LitDirs -> URI -> Path Abs File
--                -> TextDescriptor
---- construct at text state with authorDir and buchFilename as FilePath
--fillTextState4 litdirs server fp = fillTextState3 litdirs server author buch
--    where
--            author = getImmediateParentDir fp
--            buch = getNakedFileName fp


