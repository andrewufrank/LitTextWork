{-----------------------------------------------------------------------------
--
-- Module      :  Main
-- @Author@
--
-- |  a package to access the 4store http functions


-----------------------------------------------------------------------------}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Main  where

import           Uniform.Error
import           Uniform.FileIO
import           Uniform.Strings
--import EitherError.Top
--import Data.Strings
import qualified Control.Exception        as E (catch)
import           Data.RDF.Types           (RDF (..))
import qualified Data.Text                as T (lines)
import           Store.FourStore
import           Store.RDFstore.Checks
import           Store.RDFstore.Structure
--import Data.RDF.Extension
--import Text2Onto.TripleMaker
import           Data.RDF


programName = "rdf4strore"
progTitle =  " 0.1.4 jan6 demo3"

mainDebug = False

-- storeType = undefined "storeType" :: EndPointType

-------------------------------------------------------------testing
demo  = "http://127.0.0.1:8000" :: Text
demo3  = "http://127.0.0.1:8003" :: Text
db14  = "http://127.0.0.1:8012" :: Text
demo124= "http://10.0.0.124:8014" :: Text
demoUnRe= "http://127.0.0.1:8113" :: Text
demoUnReX= "http://127.0.0.1:8013" :: Text


q1 :: Text

q1 = unwords' ["Select ?s ?p ?o "
                ," where {"
                ," ?s ?p ?o "
                ,"}  "
                ]

q2 = "prefix lit: <http://auf.us.to/lit_2014#> prefix dbres: <http://dbpedia.org/resource/> prefix xsd: <http://wwww.w3.org/2001/XMLSchema#> Select ?part ?name where { ?part lit:titel ?name }"

-- a query to produce utf8 problems

q3 = unwords' ["Construct {?s ?p ?o . } "
                ," where {"
                ," ?s ?p ?o "
                ,"} "
                ] :: Text
q4 = unwords' ["Construct {?s ?p ?o ?g. } " -- does not work with 4store
                ," where { Graph ?g {"
                ," ?s ?p ?o "
                ,"} }"
                ] :: Text
--CONSTRUCT {
--  ?X vCard:FN ?name .
--  ?X vCard:URL ?url .
--  ?X vCard:TITLE ?title .
--}FROM <http://www.w3.org/People/Berners-Lee/card>
--WHERE {
--  OPTIONAL { ?X foaf:name ?name . FILTER isLiteral(?name) . }
--  OPTIONAL { ?X foaf:homepage ?url . FILTER isURI(?url) . }
--  OPTIONAL { ?X foaf:title ?title . FILTER isLiteral(?title) . }
--}


u1 :: Text
u1 = unwords' [  --`"PREFIX dc: <http://purl.org/dc/elements/1.1/>" ,
              "INSERT DATA"
            , "{  GRAPH <http://example/store> {"
            , "  <http://example/book1> " -- dc:title \"A new book\" ; "
            , "             <http://example/creator> \"A.N.Other\" ."
            , " } }"
            ]

d1 :: Text
d1 = unlines' [
            "PREFIX dc: <http://purl.org/dc/elements/1.1/>"
            , "DELETE DATA"
            , " { "
            , "  <http://example/book2> "
            , "dc:title \"David Copperfield\" ;"
            , "dc:creator \"Edmund Wells\" ."
            , " }"
            ]



--PREFIX dc: <http://purl.org/dc/elements/1.1/>
--
--DELETE DATA
--{
--  <http://example/book2> dc:title "David Copperfield" ;
--                         dc:creator "Edmund Wells" .
--}



test2 :: ErrIO ()
test2 = do
    putIOwords ["\n\nquery ", q1]
    t1 <- selectQueryText storeType demo3   Nothing Nothing  q1 -- "sparql" )  q1
    putIOwords ["q2 result on demo get\n", showT t1]
    let t2 = structureContentx t1
--    putIOwordsT ["\n\n q2 result t2\n", s2t . unlines .map unwords . (map (map show)) . either "" id $ t2]
    let t3 = fromJustNote "binding value is not ok" t2

    let t4 =  (map (res2string True)) t3
    putIOwords ["\n\n q2 result as string\n", unlines' t4]

    let t5 =   (map (res2triple)) t3
    putIOwords ["\n\n q2 result as triples\n", unlines' . map (s2t. show)  $ t5]

--    let t6 = parseString (XmlParser Nothing Nothing) t1
--                    :: Either ParseFailure (RDF TList)
--    putIOwords ["\n\n q2 result as RDF List \n", show t6]
--  produces triples, but in a representation of triples as triples of triples
    return ()

--test3construct :: ErrIO ()
---- to check a construct query to unload all of a dataset
--test3construct = do
--    t1 <- constructQueryx   demo q3  -- "sparql" )  q1
--    putIOwordsT ["test3construct q2 result on demo get"]
--    putIOwords [ t1]
----    let t2 = structureContentx t1
--    putIOwordsT ["\n\n q2 result t2", s2t $ show t1]
----    let t3 = maybe [""] (map res2string) t2
----    putIOwordsT ["\n\n test3construct q2 result as string", unlines' t3]
--    return ()

--test3unload  :: ErrIO ()
---- to unload a dataset using a select query -- needs more thinking
---- does not produce a loadable format

--test3unload = do
--    t1 <- selectQueryText storeType  demo q1  -- "sparql" )  q1
--    putIOwords ["test3unload q2 result on demo get", show t1]
--    let t2 = structureContentx t1
--    putIOwordsT ["\n\n test3unload q2 result t1", s2t $ show t2]
--    let t3 = maybe [] (map res2triple) t2
--    putIOwordsT ["\n\n test3unload q2 triples  ", unlines' .  map (s2t . show)  $ t3]
--    let tg = mkRdf t3 Nothing emptyPM
--    tgout <- writeTurtle tg "output.ttl"
--    return ()
--        where
--            q1 = unwords' ["Select ?s ?p ?o "
--                ," where {"
--                ," ?s ?p ?o "
--                ,"}  "
--                ]

--test4unload  :: ErrIO ()
---- to unload a dataset using a select query
--test4unload = do
--    let fn = "output.ttl"  :: FilePath
--    t1 <- unloadGraph storeType  demoUnReX fn (Just "unre") emptyPM
--    t2 <- readFile2 fn    :: ErrIO Text
--    putIOwords ["test3unload  ", show t2]
--    return ()
--
--test4unloadKB  :: ErrIO ()
---- to unload a dataset using a select query
--test4unloadKB = do
--    let fn = "output.ttl"  :: FilePath
--    t1 <- unloadKBx   demo
--    writeFile2 fn t1   :: ErrIO ()
--    putIOwords ["test3unload  ", show t1]
--    return ()
--
--test2u :: ErrIO ()
--test2u = do
--    putIOwordsT ["test2u start"]
--    t1 <- updateText storeType demo124 u1 -- (s2t ex4supdate2)
----    t1 <- updateTextx  (db14) u1
----    case t1x of
----        Left msg -> do
----                putIOwords ["test2u failed", msg]
----                fail msg
----        Right t1 -> do
--    putIOwords ["test2u result on demo update", show t1]
--    return ()

g1 = Just "http://auf.us.to/g1"

--test2dg :: ErrIO ()
--test2dg = do
--    putIOwordsT ["test2 delete graph  start"]
--    t1 <- deleteGraph storeType demo124 g1 -- (s2t ex4supdate2)
----    t1 <- updateTextx  (db14) u1
----    case t1x of
----        Left msg -> do
----                putIOwords ["test2u failed", msg]
----                fail msg
----        Right t1 -> do
--    putIOwords ["test2 delete graph", show t1]
--    return ()


fn1 = "/home/frank/Code/workspace/testdata/45.rdf"
fn2 = "/home/frank/Code/workspace/testdata/45.rdfxml"
fn3 = "/home/frank/Code/workspace/testdata/45.ttl"
fn4 = "/home/frank/Code/workspace/testdata/21_Nicht vor Mailand.ttl"
fn5 = "/home/frank/Code/workspace/testdata/45_Dasneue_Lied.ttl"
fn6 = "/home/frank/Code/workspace/testdata/21.ttl"
fn7 = "/home/frank/2christine/aichinger_werk/13unglaubwuerdigeReisen/13test.ttl"
fn8 = "/home/frank/Code/workspace/testdata/ttl.txt"
            -- test for non-ascii umlaut accent
fn9 = "/home/frank/Code/workspace/litText-0.1/mitCodeConv.ttl"

gr1 = Just "test2"
gr4 = Just "test4"
gr5 = Just "http://example.com/gr5"
gr6 = Just "http://example.com/gr6"
gr7 = Just "http://example.com/gr7"
nogroup = Nothing

--test2lg :: ErrIO ()    -- not working TODO
--test2lg = do
--    putIOwordsT ["test2 load graph  start"]
--    t1 <- loadGraph storeType demo fn8 nogroup-- (s2t ex4supdate2)
--    putIOwords ["test2 load graph", show t1]
--    return ()
--
--
--test2ag :: ErrIO ()
--test2ag = do
--    putIOwordsT ["test2 append graph  start"]
--    t1 <- appendGraph storeType demo fn9 gr5-- (s2t ex4supdate2)
--    putIOwords ["test2 append graph", show t1]
--    return ()
--
--
--test2del :: ErrIO ()
--test2del = do
--    putIOwordsT ["test2 deleteGraph graph  start"]
--    t1 <- deleteGraph storeType demo  gr5-- (s2t ex4supdate2)
--    putIOwords ["test2 appdeleteGraph graph", show t1]
--    return ()




---- | get the result from a computation into the IO monad
--runErr2 :: ErrIO a -> IO (ErrOrVal a)
--runErr2 a =  (runErr $ a)
--        `E.catch` \(e ::SomeException) -> return (Left . show $ e )
--
--runErr3 :: Show a => ErrIO a -> ErrIO(ErrOrVal a)
--runErr3 a = do
--                res <- liftIO     (runErr $ a)
--                putIOwords ["res in runErr3", show res]
--                return res
--
----        `E.catch` \(e ::SomeException) -> return (Left . show $ e )

-----------------------------------------------------------mains
mainErrIO ::   ErrIO ()
mainErrIO   = do
--
    putIOwords ["\n test2 query"]
    test2

--    putIOwordsT ["\n test2 construct query"]
--    test3construct

--    putIOwordsT ["\n  test3unload query"]
--    test3unload

--    putIOwordsT ["\n test4unload query"]
--    test4unload


--    putIOwordsT ["\n test2catch query x"]
----    callIO $ do
--    res <- liftIO . runErr  $ selectQueryText storeType (demo) q1
--    putIOwords  ["res found", show res ]
--    case res of
--            Left msg -> putIOwords ["caught here ", msg]
--            Right cont -> do
--                putIOwordsT ["right"]
--                let t2 = structureContentx cont
--                let t3 = maybe [""] (map (res2string True)) t2
--                putIOwordsT ["\n\n q2 result as string", unlines' t3]


--
--    putIOwordsT ["\n test2 update"]
--    test2u
--
--    putIOwordsT ["\n test2 loadGraph"]
--    test2lg

--    putIOwordsT ["\n test2 deleteGraph"]
--    test2ag

--    putIOwordsT ["\n test2 deleteGraph"]
--    test2del

--    putIOwordsT ["\n test2 unload KB "]
--    test4unloadKB

    return ()

main :: IO ()
main = do
        putIOwords [ "------------------ ", programName , progTitle, " ----------------------------"]
        r <- runErr $ mainErrIO

        putIOwords ["main", progTitle, "returning", s2t . show $ r, "-------------------------"]
--    `catchError`
--        \(e) -> throwError . unwords $ [ "SomeIOerror", "final error", show e]
