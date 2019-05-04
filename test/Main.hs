import Test.Tasty
import Test.Tasty.HUnit

import Ghci

main :: IO ()
main = defaultMain importTests

importTests :: TestTree
importTests =
    testCase "Import tests" $ do
        parseCompletion "import A" @?= Just (Module "A" (Loc 1 8 1 9))
        parseCompletion "import qualified A" @?= Just (Module "A" (Loc 1 18 1 19))
        parseCompletion "import qualified A (" @?= Just (ModuleExport "A" "" (Loc 1 20 1 21))
        parseCompletion "import qualified A (a" @?= Just (ModuleExport "A" "a" (Loc 1 21 1 22))
        parseCompletion "import qualified A (a," @?= Just (ModuleExport "A" "" (Loc 1 22 1 23))
        parseCompletion "import qualified A (a, b" @?= Just (ModuleExport "A" "b" (Loc 1 24 1 25))
        parseCompletion "import" @?= Just (Module "" (Loc 1 1 1 7))
        parseCompletion "import " @?= Just (Module "" (Loc 1 1 1 7))
        parseCompletion "import qualified" @?= Just (Module "" (Loc 1 8 1 17))
        parseCompletion "import qualified " @?= Just (Module "" (Loc 1 8 1 17))
