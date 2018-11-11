module Refactor.MoveForward.TopLevel.MultipleDefinitionSwapParams where

f :: Int -> String -> Bool
f 1 "a" = True
f 2 "b" = True
f y x   = False
