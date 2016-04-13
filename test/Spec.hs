import Test.Hspec

import qualified Youtan.Regex.NDFMTest as NDFM ( spec )
import qualified Youtan.Regex.OperatorsTest as Operators ( spec )

main :: IO ()
main = hspec $ parallel $ do
  describe "NDFM" NDFM.spec
  describe "Operators" Operators.spec

