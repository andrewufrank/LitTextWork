-- #!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle

-----------------------------------------------------------------------------
--
-- Module      :  collect all markup files and process them
-- Copyright   :  andrew u frank -
--
-- | takes cmd line arguments and converts markup to nt
-- takes   cmd line argument: switch -o for original default test
-- flags -d --author_dir -f --buch_file
-- later: language (to process the english files first and then the other languages
-- in turn

-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Processor.ProcessAll hiding ((<>) , (</>), (<.>))
--import           Parser.Foundation ()
--     hiding ((<>) , (</>), (<.>))
-- import           Parser.LinesToParagrahs
--import           Uniform.FileIO         hiding ((<>))
--import           Uniform.Strings              hiding ((<>), (</>), (<.>))
--import Lines2para.Lines2ignore (LanguageCode, readLanguageCode)
--            hiding ((<>) , (</>), (<.>))

import           Uniform.Convenience.StartApp hiding ((<>) , (</>), (<.>))

import           Data.Semigroup               ((<>))
import           Options.Applicative.Builder
import           Options.Applicative


programName = "may13 = ProduceLit" :: Text
progTitle = "produce the lit for all markup files " :: Text

main :: IO ()
main = do
    startProg programName progTitle
        (parseAndExecute
               (unlinesT ["gets the nt files" ]
               )
        "orig/test"
        )
resfile  = makeRelFile "resultCollectAll"

--- cmd line parsing
data LitArgs = LitArgs
  {  argOrigTest   :: Bool   -- ^ orig or test to decide where to take the file
  , argdir   :: String   -- ^ the subdirectory in originals
                        -- where the markup file is
                        -- the same dirname is used in convertsDir
  , argbuch  :: String -- ^ the filename in the dir
--  , argLanguage :: String -- ^ the languages of the processed files
                -- others will be sidestepped
--  , argGraph  :: String  -- ^ the graph
  }

cmdArgs :: Parser (LitArgs)
cmdArgs = LitArgs
     <$> switch
          ( long "orig" <>
            short 'o' <>
--            metavar "orig/test" <>
            help "orig or test" )
     <*> strOption
          ( long "author(dir)" <>
            short 'd' <>
            value "" <>
            metavar "Author"
            <> help "subdirectory name in LitOriginal - author" )
     <*> strOption
          ( long "buch(file)" <>
            short 'f' <>
            value "" <>
            metavar "buch"
            <> help "buch - filename " )



parseAndExecute  :: Text -> Text ->  ErrIO ()
parseAndExecute t1 t2    = do
        args <- callIO $ execParser opts
        let resfile  = makeRelFile "resultCollect"
--        let lang = readLanguageCode "readLangCode in MainCollect"
--                    (s2t $ argLanguage args) :: LanguageCode

        let generality  = if argOrigTest args
                then generalityOrig4
                else generalityTest4
        let source = if argOrigTest args
                then sourceOrig4
                else sourceTest4
        if  null (argdir args)
            then
                processAll False  source  generality  resfile
            else do
                let textstate = fillTextState2 source generality
                         (argdir args) (argbuch args)
                mainLitAndNLPproduction False  textstate
                return ()
--        processAll False dir (s2t . argDB $ args) (Just . s2t $ argGraph args) resultFile
--        -- true for debug stores only the first 3 triples...
        return ()
      where
        opts = info (helper <*> cmdArgs)
          ( fullDesc
         <> (progDesc . t2s $ t1)
         <> (header . t2s $ t2 ))
