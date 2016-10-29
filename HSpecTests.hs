module Main where
 
import Restle
import Test.Hspec
import Network.Wai
import Network.Wai.Test
--import Network.Wai.Internal (Request (Request))
import Network.HTTP.Types

main :: IO ()
main = hspec $ do
 
  describe "Low-level request-response" $ do
    it "does basic" $ do
        let request_ = setPath defaultRequest "/"
        response_ <- sendRequest request_
        let status = responseStatus response_
        statusCode status `shouldBe` 200
    it "does 404" $ do
        let request_ = setPath defaultRequest "/notfound"
        response_ <- sendRequest request_
        let status = responseStatus response_
        statusCode status `shouldBe` 404

  describe "followRequest" $ do
    it "wut" $ do
        let page = Page {
            links = [
                Link
                    { linkRel = "people"
                    , href  = "/people"
                }
            ],
            forms = []
        } in let r = "people"
            in pathInfo (followRequest page r) `shouldBe` ["people"]
