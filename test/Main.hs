import Data.List (sort)
import GHC.SyntaxHighlighter
import Language.Haskell.Ghcid

import Test.Tasty
import Test.Tasty.HUnit

import Complete
import Parse

main :: IO ()
main = defaultMain $ testGroup "All tests" [importTests, ghciLoadTest, completionTests]

ghciLoadTest :: TestTree
ghciLoadTest =
    withResource
        (fst <$> startGhci "ghci Main.hs ModA.hs" (Just "test/stub") (\_ _ -> return ()))
        stopGhci
        (\getGhci ->
             testGroup
                 "Ghcid test"
                 [ testCase "Load code" $ do
                       ghci <- getGhci
                       mods <- showModules ghci
                       sort mods @?= sort [("Main", "Main.hs"), ("ModA", "ModA.hs")]
                 , testCase "Complete code" $ do
                       ghci <- getGhci
                       compl1 <- performCompletion ghci Nothing (Variable "foo" undefined)
                       (case compl1 of
                            Just ([Candidate {candidate = "foo", type_ = ":: String"}], False) ->
                                True
                            _ -> False) @?
                           ("Unexpected: " ++ show compl1)
                       compl2 <- performCompletion ghci Nothing (Variable "ModA.ba" undefined)
                       (case compl2 of
                            Just ([Candidate {candidate = "ModA.bar", type_ = ":: Int"}], False) ->
                                True
                            _ -> False) @?
                           ("Unexpected: " ++ show compl2)
                 ])

completionTests :: TestTree
completionTests =
    withResource
        (fst <$> startGhci "cabal new-repl" Nothing (\_ _ -> return ()))
        stopGhci
        (\getGhci ->
             testCase "Completion test" $ do
                 ghci <- getGhci
                 compl <- performCompletion ghci Nothing (Module "Data.Bool" undefined)
                 (case compl of
                      Just ([Candidate {candidate = "Data.Bool", type_ = ""}], False) -> True
                      _ -> False) @?
                     "Unexpected result")

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
