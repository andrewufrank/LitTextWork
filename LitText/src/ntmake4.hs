-- #!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle

-----------------------------------------------------------------------------
--
-- Module      :  collect all markup files and process them
-- Copyright   :  andrew u frank -
--
-- | takes cmd line arguments and converts markup to nt
-- takes   cmd line argument: -s gives source dir fuer die markup files.
-- later : -f for the filename
-- later: language (to process the english files first and then the other languages
-- in turn

-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Pipes as Pipe
import qualified Pipes.Prelude as Pipe
import Pipes ((>->), (~>))

--import Processor.ProcessAll hiding ((<>) , (</>), (<.>))
--import           Parser.Foundation ()
--     hiding ((<>) , (</>), (<.>))
-- import           Parser.LinesToParagrahs
import           Uniform.FileIO         hiding ((<>))
--import           Uniform.Strings              hiding ((<>), (</>), (<.>))
--import Lines2para.Lines2ignore (LanguageCode, readLanguageCode)
--            hiding ((<>) , (</>), (<.>))

import           Uniform.Convenience.StartApp hiding ((<>) , (</>), (<.>))

import           Data.Semigroup               ((<>))
import           Options.Applicative.Builder
import           Options.Applicative
import Producer.Servers
import Parser.TextDescriptor hiding ((<>))
import          Data.RDF.FileTypes (ntFileTriples,ntFileTriplesGZip)
import Processor.Main2sub (mainLitAndNLPproduction)


programName = "ntmake" :: Text
progTitle = "produce the lit for all markup files " :: Text

main :: IO ()
main = startProg programName progTitle
            ( parseAndExecute
               (unlinesT ["gets the nt files" ])
               "dir relative to home"
            )

--- cmd line parsing
data LitArgs = LitArgs
  {
    argSource :: String -- ^ the directoy in which the markup files are
--  , argdir   :: String   -- ^ the subdirectory in originals
--                        -- where the markup file is
--                        -- the same dirname is used in convertsDir
--  , argbuch  :: String -- ^ the filename in the dir
  }

cmdArgs :: Parser (LitArgs)
cmdArgs = LitArgs
     <$> strOption
        (long "sourcedir" <>
            short 's'  <>
            help "dir in which the markup files are (relative to home)" )
--      <*> switch
--          ( long "localhost" <>
--            short 'l' <>
----            metavar "orig/test" <>
--            help "localhost or serverBrest" )
--     <*> strOption
--          ( long "author(dir)" <>
--            short 'd' <>
--            value "" <>
--            metavar "Author"
--            <> help "subdirectory name in LitOriginal - author" )
--     <*> strOption
--          ( long "buch(file)" <>
--            short 'f' <>
--            value "" <>
--            metavar "buch"
--            <> help "buch - filename " )



parseAndExecute  :: Text -> Text ->  ErrIO ()
parseAndExecute t1 t2    = do
        args <- callIO $ execParser opts
--        let lang = readLanguageCode "readLangCode in MainCollect"
--                    (s2t $ argLanguage args) :: LanguageCode

        let homedir = makeAbsDir "/home/frank" :: Path Abs Dir
        let markupdir = addDir homedir (argSource args :: FilePath) :: Path Abs Dir
        let resfile  = addFileName homedir $ makeRelFile "resultCollect" :: Path Abs File
        let ntdir = addDir homedir ("NT" :: FilePath)  :: Path Abs Dir
        let authorReplacement = s2t . getNakedDir . argSource $ args
        processAll4 False markupdir serverBrest ntdir resfile authorReplacement
--
--        let server = if  argLocalhost args
--                    then serverLocalhost
--                    else serverBrest
--        if  null (argdir args)
--            then
--                processAll False  dirs  server  resfile
--            else do
--                let textstate = fillTextState3 dirs server
--                         (argdir args) (argbuch args)
--                mainLitAndNLPproduction False False textstate
                -- first bool is debug output
                -- second stops processing after lit (no nlp calls)
--                return ()
--        processAll False dir (s2t . argDB $ args) (Just . s2t $ argGraph args) resultFile
--        -- true for debug stores only the first 3 triples...
        return ()
      where
        opts = info (helper <*> cmdArgs)
          ( fullDesc
         <> (progDesc . t2s $ t1)
         <> (header . t2s $ t2 ))

processAll4 :: Bool ->  Path Abs Dir-> URI -> Path Abs Dir  -> Path Abs File  -> Text -> ErrIO ()
-- | get all markup files in the partially filled TextState2
-- the output goes to the second, not the triples
-- debug flag is not yet used

processAll4 debug path  server ntdir resfile authorReplacement =
--  let path = source litdirs
  bracketErrIO (openFile2handle resfile WriteMode)
        (closeFile2)
        (\hand ->
              Pipe.runEffect $
                getRecursiveContents path
                >-> Pipe.filter isMarkup --
                >-> Pipe.mapM (fmap t2s . processOneMarkup4 debug server ntdir authorReplacement)
            --    >-> P.stdoutLn
                >-> Pipe.toHandle hand
        )

isMarkup :: Path Abs File -> Bool
isMarkup  = hasExtension (Extension "markup")
-- todo include in typedfiles - hasType ...

processOneMarkup4 :: Bool  -> URI -> Path Abs Dir -> Text -> Path Abs File
            -> ErrIO Text
-- process one markup file, if the nt file does not exist
processOneMarkup4 debug   server ntdir  authorReplacement file = do
        let buchReplacement = s2t $ getNakedFileName file
        let textstate2 = fillTextState4a file server ntdir authorReplacement buchReplacement
        putIOwords ["\nprocessOneMarkup", showT textstate2  ]
        ntgzExist <- exist6 (destNT textstate2) ntFileTriplesGZip
        ntExist <- exist6 (destNT textstate2) ntFileTriples
        -- gzipFlag textstate2

        if not ntExist
            then do
                putIOwords  ["\nprocessMarkup - process"
                        , showT $ sourceMarkup textstate2, "\n"]
                mainLitAndNLPproduction False False textstate2
                -- first bool is debug output
                -- second stops processing after lit (no nlp calls)
                putIOwords  ["\nprocessMarkup - processed"
                        , showT $ sourceMarkup textstate2, "\n"]
                return (showT textstate2)
            else do
                putIOwords  ["\nprocessMarkup - nt file exist already"
                        , showT $ sourceMarkup textstate2, "\n"]
                return . unlinesT $ ["\nprocessMarkup - nt file exist already"
                        , showT $ sourceMarkup textstate2, "\n"]

    `catchError` \e -> do
            putIOwords  ["\nprocessMarkup - error return for "
                    , showT file, "\n"
                    , "error", e
                    , "not raised further to continue processing all"
                    ]
            return ""
