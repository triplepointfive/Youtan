import Test.Hspec

import qualified Youtan.Regex.DFMTest as DFM ( spec )
import qualified Youtan.Regex.NDFMTest as NDFM ( spec )
import qualified Youtan.Regex.OperatorsTest as Operators ( spec )
import qualified Youtan.Lexical.TokenizerTest as Tokenizer ( spec )
import qualified Youtan.Syntax.CFGTest as CFG ( spec )

main :: IO ()
main = hspec $ parallel $ do
  describe "DFM" DFM.spec
  describe "NDFM" NDFM.spec
  describe "Operators" Operators.spec
  describe "Tokenizer" Tokenizer.spec
  describe "CFG" CFG.spec
