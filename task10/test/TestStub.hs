import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testsStub

testsStub :: TestTree
testsStub= let
        a = 2
        b = 2
        c = 4
    in testGroup "Unit test stub"
        [ testCase "2+2=4" $
            a + b @?= c
        , testCase "2*2=4" $
            a * b @?= c
        ]
