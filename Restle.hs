module Restle where

import Network.Wai
import Network.Wai.Test
import Network.HTTP.Types
import Data.ByteString.Char8 (pack, unpack)
import Data.Map (Map)
import Control.Monad.State


data Link = Link
    { linkRel :: Rel
    , href :: String}

data Form = Form
    { formRel :: Rel
    , action :: String
    , defaultData :: Map String [String]}

data Page = Page
    { links :: [Link]
    , forms :: [Form]}

type Rel = String


--type ServerState = State
--type ClientState = State


--represent :: Response -> ClientState
--represent =
--
--
--respond :: ServerState -> Request -> (ServerState, ClientState)
--respond serverState r =
--    represent $ sendRequest r


app :: Application
app request respond = do
    putStrLn "I've done some IO here"
    putStrLn $ unpack (rawPathInfo request)
    respond $ case rawPathInfo request of
        "" -> index
        "/" -> index
        _ -> notFound


index :: Response
index = responseLBS
        status200
        [("Content-Type", "text/plain")]
        "Hello, Web!"


notFound :: Response
notFound = responseLBS
    status404
    [("Content-Type", "text/plain")]
    "404 - Not Found"


sendRequest :: Request -> IO Response
sendRequest request_ = do
    srsp <- runSession (request request_) app
    let rsp = responseLBS (simpleStatus srsp) (simpleHeaders srsp) (simpleBody srsp)
    return rsp

matchesLinkRel :: Rel -> Link -> Bool
matchesLinkRel r l = linkRel l == r

matchesFormRel :: Rel -> Form -> Bool
matchesFormRel r f = formRel f == r

followRequest :: Page -> Rel -> Request
followRequest p r =
    setPath defaultRequest (pack hr)
    where hr = href $ last $ filter (matchesLinkRel r) (links p)


queryRequest :: Page -> Rel -> (Map String [String]) -> Request
queryRequest p r d =
    setPath defaultRequest (pack hr)
        where hr = action $ last $ filter (matchesFormRel r) (forms p)
