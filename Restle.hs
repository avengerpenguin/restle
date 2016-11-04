module Restle where
    
import Network.Wai
import Network.Wai.Test
import Network.HTTP.Types
--import Data.ByteString.Char8 (pack, unpack)
import Data.Map (Map)
import Control.Monad.State
import Data.RDF
import qualified Data.Text as T
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy.UTF8 as UTF8Lazy
import Blaze.ByteString.Builder (toByteString, Builder, fromWord8)
import Data.IORef
import Control.Monad.State
import Data.Maybe

type Graph = RDF TList

type Path = T.Text
type Rel = T.Text
type URI = T.Text

--type ServerState = Graph
--type ClientStateTransitions = Graph

--type Server = (Service, State Graph)
data Server = Server
    { service :: Service
    , serverState :: Graph}

type Client = Graph
                  
type Handler = Server -> Path -> Method -> Maybe Graph -> State Server Client

data Service = Service [(Method, Path, Handler)]


--query :: Client -> Rel -> Graph -> State Server Client
--
--followLink :: Client -> Rel -> State Server Client
--
--request :: Server -> Path -> Method -> Maybe Graph -> State Server Client


--data Service = Service
--    { serviceTransitions :: [
--        ( Method
--        , Path
--        , Handler
--        )
--    ]}

--data ClientState = ClientState
--    { entityData :: Graph
--    , clientTransitions :: [(Rel, URI, Maybe Graph)]}


transitionMatch method_ path_ (m, p, h) =
    (method_ == m) && (path_ == p)


enter :: State Server Client
enter = do
  server <- get
  let Service routes = service server
    in let (_, _, handler) = head $ filter (transitionMatch methodGet "/") routes
      in handler server "/" methodGet Nothing
                  

follow :: Client -> Rel -> [State Server Client]
follow client rel = do
  let triples = query client Nothing (Just $ unode rel) Nothing
  flip map triples $ \(Triple s p o) -> do
      server <- get
      let UNode path = o
        in let Service routes = service server
          in let (_, _, handler) = head $ filter (transitionMatch methodGet path) routes
           in handler server path methodGet Nothing
  

followFirst client rel = head $ follow client rel

makeLinks :: Path -> [(T.Text, T.Text)] -> Client
makeLinks path [] = empty
makeLinks path ((rel, href):xs) = addTriple (makeLinks path xs) $ triple (unode path) (unode rel) (unode href)
              

union :: Graph -> Graph -> Graph
union g1 g2 = foldr (\triple gr -> addTriple gr triple) g1 ts
  where ts = triplesOf g2

--enter server = handler ((serverState server), (clientStateTransitions server))
--    where (_, _, handler) = head $ filter (transitionMatch methodGet "/") (serviceTransitions (service server))


--makeData :: [(Path, [(Rel, URI)])] -> RDF TList
--makeData [] = empty
--makeData ((p, rus):xs) = foldr (\triple gr -> addTriple gr triple) (makeData xs) ts
--    where ts = map (\(r, u) -> triple (unode p) (unode r) (unode u)) rus
--
--
--doGet :: (RDF TList) -> (ServerState, ClientStateTransitions) -> (ServerState, ClientStateTransitions, ClientState)
--doGet entityData_ (serverState_, clientStateTransitions_) =
--    (serverState_, clientStateTransitions_, client_)
--    where client_ = ClientState {entityData = entityData_, clientTransitions = []}


--data Link = Link
--    { linkRel :: Rel
--    , href :: String}
--
--data Form = Form
--    { formRel :: Rel
--    , action :: String
--    , defaultData :: Map String [String]}
--
--data Page = Page
--    { links :: [Link]
--    , forms :: [Form]
--    , graph :: RDF TList}
--
--type Rel = String
--

--data Resource = Resource {
--    path :: String
--}
--
--data Server = Server
--    { entrypoint :: Resource
--    }
--
--data Client = Client {}
--
--enter :: String -> IO Client
--enter server = let client = Client {}
--                    in do
--                        client <- receiveState
--                        return client

--type Server = RDF

--enter :: Server -> Client
--enter = query server (Just (unode "/")) Nothing

--interpret :: Response -> IO Page
--interpret response_ =
--        let getBody res = do
--                let (_, _, f) = responseToStream res
--                f $ \streamingBody -> do
--                    builderRef <- newIORef mempty
--                    let add :: Builder -> IO ()
--                        add b = atomicModifyIORef builderRef $ \builder ->
--                            (builder `mappend` b, ())
--                        flush :: IO ()
--                        flush = return ()
--                    streamingBody add flush
--                    fmap toByteString $ readIORef builderRef
--                    in do
--                        body <- (getBody response_)
--                        let result = parseString (TurtleParser (Just (BaseUrl ("http://example.com/"))) Nothing) $ T.pack $ UTF8.toString body
--                            in case result of
--                                Left err -> error "Cannot parse response"
--                                Right rdfGraph -> return Page {graph=rdfGraph}
--
--
--enter server =
--    let path = entrypoint server
--        in sendRequest
--
--represent :: RDF TList -> Response
--represent triples =
--    responseLBS
--        status200
--        [("Content-Type", "text/turtle")]
--        (UTF8Lazy.fromString (showGraph triples))
--
--handle :: RDF TList -> T.Text -> Response
--handle graph path =
--    let empty_ = empty :: RDF TList
--        in let triples = query graph (Just (unode path)) Nothing Nothing
--            in let triples' = foldr (\triple gr -> addTriple gr triple) empty_ triples
--                in represent triples'
--
--
--makeServer :: RDF TList -> Application
--makeServer graph request_ respond = do
--        respond $ case rawPathInfo request_ of
--            "" -> handle graph "/" -- hack because "" comes in instead of /
--            _  -> handle graph $ T.pack $ UTF8.toString (rawPathInfo request_)

--type ServerState = State
--type ClientState = State


--represent :: Response -> ClientState
--represent =
--
--
--respond :: ServerState -> Request -> (ServerState, ClientState)
--respond serverState r =
--    represent $ sendRequest r


--app :: Application
--app request_ respond = do
--    putStrLn "I've done some IO here"
--    putStrLn $ UTF8.toString (rawPathInfo request_)
--    respond $ case rawPathInfo request_ of
--        "" -> index
--        "/" -> index
--        _ -> notFound
--
--
--index :: Response
--index = responseLBS
--        status200
--        [("Content-Type", "text/plain")]
--        "Hello, Web!"
--
--
--notFound :: Response
--notFound = responseLBS
--    status404
--    [("Content-Type", "text/plain")]
--    "404 - Not Found"


--sendRequest :: Application -> Request -> IO Response
--sendRequest app_ request_ = do
--    srsp <- runSession (request request_) app_
--    let rsp = responseLBS (simpleStatus srsp) (simpleHeaders srsp) (simpleBody srsp)
--    return rsp
--
--
--matchesLinkRel :: Rel -> Link -> Bool
--matchesLinkRel r l = linkRel l == r
--
--
--matchesFormRel :: Rel -> Form -> Bool
--matchesFormRel r f = formRel f == r
--
--
--followRequest :: Page -> Rel -> Request
--followRequest p r =
--    setPath defaultRequest (UTF8.fromString hr)
--    where hr = href $ last $ filter (matchesLinkRel r) (links p)
--
--
--queryRequest :: Page -> Rel -> (Map String [String]) -> Request
--queryRequest p r d =
--    setPath defaultRequest (UTF8.fromString hr)
--        where hr = action $ last $ filter (matchesFormRel r) (forms p)
