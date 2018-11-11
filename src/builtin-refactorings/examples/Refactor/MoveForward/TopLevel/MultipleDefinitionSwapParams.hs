module Refactor.MoveForward.TopLevel.MultipleDefinitionSwapParams where

f :: String -> Int -> Bool
f "a" 1 = True
f "b" 2 = True
f x y   = False
