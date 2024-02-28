import Test.DocTest    ( doctest )

main :: IO ()
main = doctest $ concat
  [ map ("-X" ++)
    -- All language extensions needed by the tests
    [ "LambdaCase"
    , "InstanceSigs"
    ]
    -- Files containing doctests
  , [ "src/Data/BST/Internal.hs"
    ]
  ]
