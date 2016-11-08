module Main where
 
import Restle
import Test.Hspec
import Network.Wai
import Network.Wai.Test
--import Network.Wai.Internal (Request (Request))
import Network.HTTP.Types
import Data.RDF
import Control.Monad.State


indexHandler :: Handler
indexHandler server "/" methodGet _ = do
  let links = makeLinks "/" [
                ("/vocab/thing", "/things/3354")
               ]
  let theData = empty
  let client = union theData links
  return client

berlinHandler :: Handler
berlinHandler server "/things/3354" methodGet _ = do
  let client = empty
  return client

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

    describe "basic full app" $ do
      it "uhhh" $ do
        let client = evalState enter myServer
          in length (triplesOf client) `shouldBe` 1

      it "does more" $ do
        let client = flip evalState myServer $ do
                         client <- enter
                         client <- followFirst client "/vocab/thing"
                         return client
        length (triplesOf client) `shouldBe` 0
