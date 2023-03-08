import qualified Basic
import qualified Strategy
import qualified UnitTest

main :: IO ()
main = do
  UnitTest.main
  Strategy.main
