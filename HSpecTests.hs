module Main where
 
import Restle
import Test.Hspec
import Network.Wai
import Network.Wai.Test
--import Network.Wai.Internal (Request (Request))
import Network.HTTP.Types
import Data.RDF
import Control.Monad.State


--server = Server {
--                entrypoint = Resource {
--                    path = "/"
--                }
--            }

--server = empty :: RDF TList
--server = addTriple empty $ triple (unode "/") (unode "http://schema.org/headline") (lnode "Hello, World!")
--app = makeServer server

indexHandler :: Handler
indexHandler server "/" methodGet _ = do
    let client = empty
    return client

berlinHandler :: Handler
berlinHandler server "/things/3354" methodGet _ = do
    let client = empty
    return client

--indexHandler :: (ServerState, ClientStateTransitions) -> (ServerState, ClientStateTransitions, ClientState)
--indexHandler (serverState_, clientStateTransitions_) =
--    (serverState_, clientStateTransitions_, client_)
--    where client_ = ClientState {entityData = empty, clientTransitions = []}


myService :: Service
myService = Service [
        (methodGet, "/", indexHandler),
        (methodGet, "/things/3354", berlinHandler)
    ]

myServer :: Server
myServer = Server
    { service = myService
    , serverState = empty}


main :: IO ()
main = hspec $ do

--  describe "sendRequest: Low-level request-response" $ do
--    it "does basic" $ do
--        let request_ = setPath defaultRequest "/"
--        response_ <- sendRequest app request_
--        let status = responseStatus response_
--        statusCode status `shouldBe` 200
--    it "does 404" $ do
--        let request_ = setPath defaultRequest "/notfound"
--        response_ <- sendRequest app request_
--        let status = responseStatus response_
--        statusCode status `shouldBe` 404
--
--  describe "followRequest" $ do
--    it "wut" $ do
--        let page = Page {
--            links = [
--                Link
--                    { linkRel = "people"
--                    , href  = "/people"
--                }
--            ],
--            forms = []
--        } in let r = "people"
--            in pathInfo (followRequest page r) `shouldBe` ["people"]

    describe "basic full app" $ do
        it "uhhh" $ do
            let client = evalState enter myServer
                in length (triplesOf client) `shouldBe` 0
            --let client = evalState (enter myServer) myServer
            --    in length (triplesOf client) `shouldBe` 0
            --(server', client') <- transition server client $ "GET" "people" None

        it "does more" $ do
            let client = flip evalState myServer $ do client <- enter
                                                      return client
            length (triplesOf client) `shouldBe` 0