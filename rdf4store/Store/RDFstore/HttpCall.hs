
 -----------------------------------------------------------------------------
--
-- Module      :  Store.RDFstore.HttpCall
--
-- | using http simple to sparql queries and to create requests
-- part of uniform (to use only text

-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
-- {-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Store.RDFstore.HttpCall (
    callHTTP7
--    , callHTTP6
    , parseURLchecked
    , Net.RequestMethod (..)
    , makeHTTPrequest5
--    , makeHTTPrequest6
    , makeHTTPgetrequestNoBody
    , urlEncode
    , urlEncodeVarsT
    , Http.RequestMethod (..)
            )  where


import           Uniform.Error
import           Uniform.Strings
--
import qualified Data.ByteString.Char8 as B
import qualified Data.Text.Encoding    as E
--
import qualified Network.HTTP          as Http
import qualified Network.HTTP          as Net
import qualified Network.URI           as NetURI

import Data.Text (take)

--debugHTTP = False  no real change

--bb2t :: ByteString -> Text
--bb2t = fromJustNote "bytestring conversionm bb2t" . b2t

-- new simplified version with more error reported
callHTTP7 :: Bool -> Http.Request ByteString -> ErrIO  Text
-- | executes the HTTP call (e.g. simpleHTTP) and deals with first level of failure
-- the return is a text, decoded the same way the makeNLPrequest was made
-- probably merge the two functions
callHTTP7 debugHTTP request = do
      res <-   _callHTTP6 debugHTTP request
      let res2 = bb2t res
      return res2


-- new simplified version with more error reported
_callHTTP6 :: Bool -> Http.Request ByteString -> ErrIO  ByteString
-- | executes the HTTP call (e.g. simpleHTTP) and deals with first level of failure
-- the return is a bytstring to decode with the same encoding used for the call
_callHTTP6 debugHTTP request = do
        when debugHTTP $
            putIOwords ["callHTTP6 : "
                    , "\nrequest", showT request
                    , "requestbody",  bb2t $ Net.rqBody request]
        res <- callIO $
                do
                    res1 <- Http.simpleHTTP request
                    when debugHTTP $
                        putIOwords ["callHTTP6 result is is", showT res1]
                    return res1
                 `catchError` \e -> do
                             putIOwords ["callHTTP6 http.simpleHTTP error caught 3", showT e
                                    , "\n should not occur - caught by callIO ??"
                                    , "\n note hint: replace localhost by 127.0.0.1"
                                    ,  "\n", showT request]
                             fail . unwords $  [ "callHTTP6 httperror 3", show e]
                                             -- is in the IO monad, not ErrIO
        case res of
             Left msg -> do
                     putIOwords ["callHTTP6 reported error ", showT msg, "for request \n", showT request]
                     throwErrorT ["callHTTP6 http select 2", showT msg, "for request \n", showT request]
             Right response -> do
                     when debugHTTP $ putIOwords ["callHTTP6 produced response 1"]
                     res3 <- callIO $ do
                                 code1 <- Http.getResponseCode res
                                 when debugHTTP $ putIOwords ["callHTTP6  getResponseCode 6", showT code1]
                                 body1 <- Http.getResponseBody res
                                 let body2 =  body1  -- change for 6
                                 when debugHTTP $ putIOwords ["callHTTP6  getResponseBody 6", bb2t body2]
                                 if fst3 code1 == 2
                                    then do
                                         when debugHTTP $ putIOwords ["callHTTP6  return ok with body", showT code1, bb2t body2]
                                         return . Right $ body2 --
--                                                   -- the result is parsed! $ unwords' [showT res, "with body", body2]
                                    else do
                                         putIOwords ["callHTTP6  return Left code", showT code1
                                                        , "for request \n", showT request]
                                         return . Left . unwords $ ["callHTTP6 returns code", show  code1, bb2s body2
                                                    , "for request \n", show  request]
                              `catchError` \e -> do
                                 putIOwords ["callHTTP6 error in getResponseBody 5 100 char"
                                        , Data.Text.take 100 . showT $ e, "for request \n", showT request]
                                 return . Left . unwords $   [ "callHTTP6 httperror 5", show e
                                                , "for request \n", show  request]
                     case res3 of
                        Left msg2 -> throwError . s2t $ msg2
                        Right b -> return (b ::ByteString)

     `catchError` (\e -> do
             putIOwords ["callHTTP6 error caught 7",  e, "for request \n", showT request] -- " showT msg])
             throwError e
                )

---- new simplified version with more error reported
--callHTTP5 :: Bool -> Http.Request ByteString -> ErrIO  Text
---- | executes the HTTP call (e.g. simpleHTTP) and deals with first level of failure
--callHTTP5 debugHTTP request = do
--        when debugHTTP $
--            putIOwords ["callHTTP5 : "
--                    , "\nrequest", showT request
--                    , "requestbody",  bb2t $ Net.rqBody request]
--        res <- callIO $
--                do
--                    res1 <- Http.simpleHTTP request
--                    when debugHTTP $
--                        putIOwords ["callHTTP5 result is is", showT res1]
--                    return res1
--                 `catchError` \e -> do
--                             putIOwords ["callHTTP5 http.simpleHTTP error caught 3", showT e
--                                    , "\n should not occur - caught by callIO ??"
--                                    , "\n note hint: replace localhost by 127.0.0.1"
--                                    ,  "\n", showT request]
--                             fail . unwords $  [ "callHTTP5 httperror 3", show e]
--                                             -- is in the IO monad, not ErrIO
--        case res of
--             Left msg -> do
--                     putIOwords ["callHTTP5 reported error ", showT msg, "for request \n", showT request]
--                     throwErrorT ["callHTTP5 http select 2", showT msg, "for request \n", showT request]
--             Right response -> do
--                     when debugHTTP $ putIOwords ["callHTTP5 produced response 1"]
--                     res3 <- callIO $ do
--                                 code1 <- Http.getResponseCode res
--                                 when debugHTTP $ putIOwords ["callHTTP5  getResponseCode 6", showT code1]
--                                 body1 <- Http.getResponseBody res
--                                 let body2 = bb2t body1
--                                 when debugHTTP $ putIOwords ["callHTTP5  getResponseBody 6", body2]
--                                 if fst3 code1 == 2
--                                    then do
--                                         when debugHTTP $ putIOwords ["callHTTP5  return ok with body", showT code1, body2]
--                                         return . Right $ body2 --
----                                                   -- the result is parsed! $ unwords' [showT res, "with body", body2]
--                                    else do
--                                         putIOwords ["callHTTP5  return Left code", showT code1
--                                                        , "for request \n", showT request]
--                                         return . Left . unwords $ ["callHTTP5 returns code", show  code1, t2s body2
--                                                    , "for request \n", show  request]
--                              `catchError` \e -> do
--                                 putIOwords ["callHTTP5 error in getResponseBody 5 100 char"
--                                        , Data.Text.take 100 . showT $ e, "for request \n", showT request]
--                                 return . Left . unwords $   [ "callHTTP5 httperror 5", show e
--                                                , "for request \n", show  request]
--                     case res3 of
--                        Left msg2 -> throwError . s2t $ msg2
--                        Right b -> return b
--
--     `catchError` (\e -> do
--             putIOwords ["callHTTP5 error caught 7",  e, "for request \n", showT request] -- " showT msg])
--             throwError e
--                )

type URItext = Text

makeHTTPrequest5 :: Http.RequestMethod -> URItext -> Text -> Text -> Net.Request ByteString
-- a call to make a HTTP request with method to the URI (text)
-- content type and body
-- does not work for queries with no body
makeHTTPrequest5 method uri contentType body =
    Net.Request { Net.rqURI = fromJustNoteT ["makeHTTPrequest5 parseURI", showT uri]
                        . NetURI.parseURI . t2s $ uri
             , Net.rqHeaders =   [hAccept, hLength , hContentType]
             , Net.rqMethod = method
             , Net.rqBody =   body'

             } -- :: Net.Request.ByteString
    where
        hAccept = Net.mkHeader Net.HdrAccept "*/*" -- "application/x-www-form-urlencoded"
--                                  "application/sparql-results+xml"
        hLength =  Net.mkHeader  Net.HdrContentLength $ show ( B.length body')
        hContentType = Net.mkHeader Net.HdrContentType  (t2s contentType)
--        body' = t2s . (E.decodeLatin1 . E.encodeUtf8)  $  body -- Net.urlEncode $ t2s body -- error 409 invalid path
--        body' = t2s  body -- Net.urlEncode $ t2s body -- error 409 invalid path
        body' = E.encodeUtf8  body -- Net.urlEncode $ t2s body -- error 409 invalid path
        -- was just t2s body --

--makeHTTPrequest6 :: Http.RequestMethod -> URItext -> Text -> ByteString -> Net.Request ByteString
---- a call to make a HTTP request with method to the URI (text)
---- content type and body, body is bytestring as is the result (same encoding - i hope )
---- does not work for queries with no body
--makeHTTPrequest6 method uri contentType body =
--    Net.Request { Net.rqURI = fromJustNoteT ["makeHTTPrequest6 parseURI", showT uri]
--                        . NetURI.parseURI . t2s $ uri
--             , Net.rqHeaders =   [hAccept, hLength , hContentType]
--             , Net.rqMethod = method
--             , Net.rqBody =   body
--
--             } -- :: Net.Request.ByteString
--    where
--        hAccept = Net.mkHeader Net.HdrAccept "*/*" -- "application/x-www-form-urlencoded"
----                                  "application/sparql-results+xml"
--        hLength =  Net.mkHeader  Net.HdrContentLength $ show ( B.length body )
--        hContentType = Net.mkHeader Net.HdrContentType  (t2s contentType)
----        body' = t2s . (E.decodeLatin1 . E.encodeUtf8)  $  body -- Net.urlEncode $ t2s body -- error 409 invalid path
----        body' = t2s  body -- Net.urlEncode $ t2s body -- error 409 invalid path
----        body' = E.encodeUtf8  body -- Net.urlEncode $ t2s body -- error 409 invalid path
--        -- was just t2s body --

makeHTTPgetrequestNoBody :: URItext -> Text -> Text -> Net.Request String
makeHTTPgetrequestNoBody uri argument text =
    Net.getRequest  $ concat [ t2s uri , t2s argument , Net.urlEncode . t2s $ text]


--checkUTF8 :: B.ByteString ->   Text
---- should be total
--checkUTF8 bs =  case E.decodeUtf8' bs of
--    Right res2 ->   res2
--    Left msg ->  res3
--        where  res3 =  E.decodeLatin1 bs
--                -- decodeLatin1 should not fail!

parseURLchecked :: (MonadError m,  ErrorType m ~ Text) => Text -> m NetURI.URI
parseURLchecked uri = do
    let  urx = NetURI.parseURI $ t2s uri
    case urx of
            Nothing ->  fail . unwords $ ["URLerror" , "not a proper url " , t2s uri]
            Just uriEnc -> return uriEnc

urlEncodeVarsT:: [(Text,Text)] -> Text
urlEncodeVarsT = s2t . Net.urlEncodeVars . map (pair t2s)

urlEncode :: Text -> Text
urlEncode = s2t . Net.urlEncode . t2s

-- move TODO
fst3 (a,b,c) = a
pair f (a,b) = (f a, f b)


