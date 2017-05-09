-- #!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle
-----------------------------------------------------------------------------
--
-- Module      :  MainParse -- checking the markup

-- Copyright   :  andrew u frank -
--
-- | checks only the parser, does not store anythings
-- produces all the properties and types
-- produces sigls to check

-- todo add check for language parsed
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

--import           Uniform.FileIO
--  -- (TypedFiles (..), TypedFile(..), mkFilepath, lpX, read5)
import           Parser.Foundation            hiding ((</>))
--import           Parser.LinesToParagraphs  hiding ((</>))
--import           Parser.NLPvocabulary
--import           Parser.ProduceLit
import           Uniform.Convenience.StartApp
import           Uniform.Strings
--    -- (NLPproperty(..), NLPtype (..))
--import           CoreNLP.Defs0                (SentID0 (..), TokenID0 (..))

main = do
--    mainTest
    startProg "markPageNumbers" "check parse, find pagenumber and sigls " main1

main1 = do
    let textstate = TextState2 {
            endpoint = "http://nlp.gerastree.at:3030/marchDB/update"
            -- endpoint = "http://127.0.0.1:3030/marchDB/update"
            -- , originalsDir = mkFilepath lpX "/home/frank/testLit/"
            , originalsDir = mkFilepath lpX "/home/frank/additionalSpace/DataBig/LitOriginals"
            -- , originalsDir = mkFilepath lpX "/home/frank/additionalSpace/DataBig/LitTest"
            -- , authorDir=  "carol" -- "waterhouse"
            -- , authorDir=  "waterhouse" -- "waterhouse"
            -- , buchname =  "test3"
            -- , buchname =  "kuw"
            , authorDir=  "LaFontaine" -- "waterhouse"
            , buchname =  "7241"
            -- , authorDir=  "Boccacio" -- "waterhouse"
            -- , buchname =  "day12"
            , graph = "a1"  -- not used here
          }

    putIOwords ["ProduceLit for", showT textstate]
    main2parse False True False textstate


main2parse debugTZ debugParse debugSigl textstate = do   -- files must have a markup extension
    putIOwords ["check the markup for ", showT textstate ]

    text  :: Text <- read5 ((originalsDir textstate) </>
                        (mkFilename lpX . authorDir $ textstate))
                        (mkFilename lpX . buchname $ textstate) markupFileType5

    bomWarning text   -- the check for BOM is in MainParse only -

    let etts2 = parseMarkup text

    putIOwords ["\n\ntest markup parse "]
--
    putIOwords ["markPageNumbers count Zeilen: found ", showT . length $ etts2 , " zeieln "]
    putIOwords ["markPageNumbers count Leerzeilen", showT . countLeerzeilen $ etts2 , "leerzeieln "]
    putIOwords ["markPageNumbers count Textzeilen", showT . length
                        . filter isTextZeile $ etts2 , "textzeilen "]
    putIOwords ["markPageNumbers count MarkupZeile", showT . length
                        . filter isMarkupZeile $ etts2 , "MarkupZeilen "]
    when debugParse $ do
        putIOwords ["markPageNumbers show MarkupZeile", unlines' . map showT
                        . filter isMarkupZeile $ etts2 , "MarkupZeilen "]
        putIOwords ["markPageNumbers count Textzeilen", showT . length
                        . filter isTextZeile $ etts2 , "textzeilen "]
        putIOwords ["markPageNumbers show MarkupZeile", unlines' . map showT
                        . filter isMarkupZeile $ etts2 , "MarkupZeilen "]
--    putIOwords ["markPageNumbers done", showT . countSeiten $ etts , "pages found"]
    putIOwords ["seitenzahlen found", showT . map  readSeitenzahl . filter isSeitenzahl $ etts2]
    putIOwords ["seitenzahlen test -- lists the gaps in the seitenzahl liste", showT $ checkSeitenzahlen etts2]

--    putIOwords ["\nseitenzahlen test -average length of lines", showT .fst $ averageLengthTextLines etts2]
--    putIOwords ["seitenzahlen test -max length of lines", showT . snd $ averageLengthTextLines etts2]
    -- when debugParse $ putIOwords ["seitenzahlen test -short lines", unlines' . map showT . filter isKurzeZeile $ etts2]
--    putIOwords ["seitenzahlen test -- lists the gaps in the seitenzahl liste", showT $ checkSeitenzahlen etts]

    putIOwords ["\n\nTZ textzeilen test "]

    let tz1 = etts2tzs  etts2

    when debugTZ $ putIOwords ["textzeilen test initial conversion  \n", showList' tz1]
    let tz2 = distributePageNrs tz1
    when debugTZ $ putIOwords ["textzeilen test pagenrs distributed \n", showList' tz2]
    let tz3 = distributeLanguage tz2
    when debugTZ $ putIOwords ["textzeilen test pagenrs distributeLanguage  \n", showList' tz3]
    let tz4 = formParagraphs tz3
    when debugTZ $ putIOwords ["textzeilen test pagenrs formParagraphs  \n", showList'  tz4]
    let tz5 = markParaNr tz4
    when debugTZ $ putIOwords ["textzeilen test pagenrs markParaNr  \n", showList'  tz5]
    let tz6 = distributeHeader tz5
    when debugTZ $ putIOwords ["textzeilen test pagenrs distributeHeader  \n",
          unlines' . map (take' 40) . map showT  $ tz6]

-------------------
    when debugSigl $ do
        putIOwords ["the properties \n"
                , showList' . map mkRDFproperty $ [LanguageTag .. DependentWordform]
                , showList' . map mkRDFproperty $ [IsBuch .. AufSeite]
                , showList' . map mkRDFproperty $ [BuchIgnoreEnd .. BuchPropText]
                ]
        putIOwords ["the types \n"
                , showList' . map mkRDFtype $ [Doc .. Coreference]
                , showList' . map mkRDFtype $ [BuchIgnoreEnd .. BuchPropText]
                ]

        let tzex = headNote "an example tz"  tz6
        let parasigl1 = paraSigl textstate tzex
        let sentsigl1 = mkSentSigl parasigl1 (SentID0 1)
        putIOwords ["the sigl"
          , "\nbuchURIx", showT $ buchURIx textstate
          , "\nparaSigl", showT $ parasigl1
          , "\ninParaSigl", showT $ inParaSigl textstate tzex
          , "\n nlpURItext", nlpURItext
          , "\nSentSigl", showT $ sentsigl1
          , "\nTokenSigl", showT $ mkTokenSigl sentsigl1 (TokenID0 1)
          ]

-- markup2File = mkTypedFile :: TypedFile Text
-- -- not possible, is text file and has extension txt
-- reproFile = mkTypedFile :: TypedFile [TZ]
-- -- the reproduced file to check (not exact, shows edits (when compared to original
-- -- and interpretation (when compared to markup2

instance TypedFiles [TZ] where
    mkTypedFile = TypedFile {tpext = e}
       where e = mkExtension lpX "repro"
    write4 fp fn ft a  =  writeFileOrCreate fn2   (unparseTZs a)
        where fn2 = combineFilepath fp fn (tpext ft)    -- fp </> (fn <.> tpext ft)
    read4 fp fn tp = error "read4 missing for TZ"

-- showList' = unlines' . map showT
