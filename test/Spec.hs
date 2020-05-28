import Test.Hspec
import Lib

main :: IO()
main = hspec $ do
   describe "Champiniones" $ do
      it "poderDefensa" $ do
         poderDefensa personajeGenerico `shouldBe` 110

      it "poderAtaque" $ do
         poderAtaque personajeGenerico `shouldBe` 40

      it "desgastar" $ do
         armadura (desgastar 12 personajeDefensivo) `shouldBe`  [(10,0),(10,4),(10,7),(10,9),(10,10),(10,10),(10,10),(10,10),(10,10),(10,10)]