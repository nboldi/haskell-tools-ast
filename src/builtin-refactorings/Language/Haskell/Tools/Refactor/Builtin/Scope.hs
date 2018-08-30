module Language.Haskell.Tools.Refactor.Builtin.Scope where

import Language.Haskell.Tools.Refactor
import Control.Reference
import Data.Aeson
import qualified Name as GHC (Name,nameOccName)
import Outputable
import SrcLoc as GHC
import OccName
import Language.Haskell.Tools.AST.SemaInfoTypes

--The result of this is every time when the user runs
--GetScope command scope function will be invoked.
getScope :: QueryChoice
getScope = LocationQuery "GetScope" scope

--Gets the closest Expr to the given position and returns the scope
--of it in QueryMonad Value
scope :: RealSrcSpan -> ModuleDom -> [ModuleDom] -> QueryMonad Value
scope sp (_,mod) _
  = case selectedExpr of (_:_)-> if result == [] then queryError "Empty scope"
                                 else return $ toJSON $ scopeToString $ semanticsScope $ head result
                         _  -> queryError "No name is selected."
  where
    selectedExpr :: [Expr]
    selectedExpr = mod ^? biplateRef
    result = getMin [] (getPos sp) selectedExpr

--Returns the closest expression to the given [Int] position
--Parameters
--[Expr]: The current min value
--[Int]: Start Line and end line from which we measure the distance
--[Expr]: The list of remained expressions.
getMin :: [Expr] -> [Int] -> [Expr] -> [Expr]
getMin [] _ [] = []
getMin [] sp (x:xs) = getMin [x] sp xs
getMin [minExp] _ [] = [minExp]
getMin [minExp] sp (x:xs)
 |getDistance minExp sp > getDistance x sp = getMin [x] sp xs
 |otherwise = getMin [minExp] sp xs

--Returns the distance square from the given [Int] parameters
--Parameters:
--Expr: The expression which we would like to get distance from the second parameter
--[Int]: Start Line and end line from which we measure the distance
getDistance :: Expr -> [Int] -> Int
getDistance expr list = (exprList !! 0 - list !! 0) ^ 2 + (exprList !! 1- list !! 1) ^ 2
 where
   exprList = getPosition $ getRange expr

--Returns the start line and the end line for SrcSpan
getPosition :: SrcSpan -> [Int]
getPosition (RealSrcSpan sp) = [srcSpanStartLine sp, srcSpanEndLine sp]

--Returns the start line and the end line for RealSrcSpan
getPos :: RealSrcSpan -> [Int]
getPos sp = [srcSpanStartLine sp, srcSpanEndLine sp]

--Returns the information for usages of the names which are visible in the scope
--Result:
--[[String]]: Where the inner list can have 2 or four element
--it consists the qualified name the short name and True or False that the
--according to the name has to be used qualified or not and the possible qualification.
--When the inner list has 2 elements it consists only the first two from this.
scopeToString :: Scope -> [[String]]
scopeToString [] = []
scopeToString (x:xs) = inner x ++ scopeToString xs

--Returns the information for usages of the names which are visible in the scope
--Result:
--[[String]]: Where the inner list can have 2 or four element
--it consists the qualified name the short name and True or False that the
--according to the name has to be used qualified or not and the possible qualification.
--When the inner list has 2 elements it consists only the first two from this.
inner :: [(GHC.Name, Maybe [UsageSpec], Maybe GHC.Name)] -> [[String]]
inner [] = []
inner (x:xs) = (getNameString x : (getOccNameString x : (getUsageSpec (spec x)))) : inner xs

--Returns the short name for the first element of the tuple
getOccNameString :: (GHC.Name, Maybe [UsageSpec], Maybe GHC.Name) -> String
getOccNameString (name, _ , _) = showSDocUnsafe $ pprOccName $ GHC.nameOccName name

--Returns the qualified name for the first element of the tuple
getNameString :: (GHC.Name, Maybe [UsageSpec], Maybe GHC.Name) -> String
getNameString (name, _ , _) = qualifiedName name

--Returns the Maybe [UsageSpec] for the tuple
spec :: (GHC.Name, Maybe [UsageSpec], Maybe GHC.Name) -> Maybe [UsageSpec]
spec (_,sp,_) = sp

--Returns the information about the usage of the name in list of String
--This information is True or False according to the name has to be used
--qualified or not, and the possible qualifier.
getUsageSpec :: Maybe [UsageSpec] -> [String]
getUsageSpec a
 | Just b <- a = getSpec b
 | Nothing <- a = []

--Returns the information about the usage of the name in list of String
--This information is True or False according to the name has to be used
--qualified or not, and the possible qualifier.
getSpec :: [UsageSpec] -> [String]
getSpec [] = []
getSpec ((UsageSpec a _ c):xs) = ([show a,c] ++ getSpec xs)
