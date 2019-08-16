module Database2 where
import Control.Monad.Write 
push::Int->State [Int]
push a = State $ \xs->(a:xs)