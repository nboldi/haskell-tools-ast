{-# LANGUAGE FlexibleContexts,
             MultiWayIf,
             RankNTypes,
             TypeFamilies,
             PatternSynonyms
             #-}

{-
  NOTE: We need Decl level checking in order to gain extra information
        from the newtype and data keywords.
  NOTE: Here we implicitly constrained the type with ExtDomain.
        but we only really need HasNameInfo.

  SEE:  https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/DerivingStrategies#Thederivingstrategyresolutionalgorithm

  TODO:
  - write tests for GADTs, data instances, Generic, Lift
-}


module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Checkers.DerivingsChecker where

import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad as Ext
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Utils.TypeLookup
import Control.Reference ((^.), (!~), (&))
import Language.Haskell.Tools.Refactor as Refact hiding (Enum)
import Language.Haskell.Tools.AST

import qualified Data.Map as Map
import Control.Monad.Trans.Maybe

import qualified Name as GHC (Name)
import qualified GHC
import PrelNames
import THNames (liftClassName)

-- can be derived
isStockClass = flip elem stockClasses
stockClasses = [ eqClassName
               , ordClassName
               , ixClassName
               , showClassName
               , readClassName
               , enumClassName
               , boundedClassName
               , dataClassName
               , typeableClassName
               , genClassName
               , gen1ClassName
               , functorClassName
               , foldableClassName
               , traversableClassName
               , liftClassName
               ]

-- can be derived for newtypes even without GND
gndNotNeeded :: GHC.Name -> Bool
gndNotNeeded = flip elem gndNotNeededClasses
gndNotNeededClasses =
  [ eqClassName
  , ordClassName
  , ixClassName
  , boundedClassName
  ]

-- can be derived for newtypes with GND
gndNeeded :: GHC.Name -> Bool
gndNeeded = flip elem gndNeededClasses
gndNeededClasses =
  [ functorClassName
  , foldableClassName
  , enumClassName
  ]

-- never selected by default for newtypes (even with GND)
gndNotAllowed :: GHC.Name -> Bool
gndNotAllowed = flip elem gndNotAllowedClasses
gndNotAllowedClasses =
  [ dataClassName
  , typeableClassName
  , showClassName
  , readClassName
  , traversableClassName
  , genClassName
  , gen1ClassName
  , liftClassName
  ]

whichExtension :: GHC.Name -> Maybe Extension
whichExtension    = flip Map.lookup nameExtensionMap

nameExtensionMap = Map.fromList nameExtensions
  where nameExtensions = [ (dataClassName,        DeriveDataTypeable)
                         , (typeableClassName,    DeriveDataTypeable)
                         , (genClassName,         DeriveGeneric)
                         , (gen1ClassName,        DeriveGeneric)
                         , (functorClassName,     DeriveFunctor)
                         , (foldableClassName,    DeriveFoldable)
                         , (traversableClassName, DeriveTraversable)
                         , (liftClassName,        DeriveLift)
                         ]

chkDerivings :: CheckNode Decl
chkDerivings = conditionalAny chkDerivings'         derivingExts
           >=> conditional    chkStandaloneDeriving Ext.StandaloneDeriving

      where chkDerivings' = chkDataDecl
                        >=> chkGADTDataDecl
                        >=> chkDataInstance

            derivingExts = [ DeriveDataTypeable
                           , DeriveGeneric
                           , DeriveFunctor
                           , DeriveFoldable
                           , DeriveTraversable
                           , DeriveLift
                           , DeriveAnyClass
                           , GeneralizedNewtypeDeriving
                           , DerivingStrategies
                           ]


chkDataDecl :: CheckNode Decl
chkDataDecl d@(DataDecl keyw _ _ _ derivs) = do
  annList !~ separateByKeyword keyw $ derivs
  return d
chkDataDecl d = return d

chkGADTDataDecl :: CheckNode Decl
chkGADTDataDecl d@(GADTDataDecl keyw _ _ _ _ derivs) = do
  addOccurence_ GADTs d
  annList !~ separateByKeyword keyw $ derivs
  return d
chkGADTDataDecl d = return d

chkDataInstance :: CheckNode Decl
chkDataInstance d@(DataInstance keyw _ _ derivs) = do
  addOccurence_ TypeFamilies d
  annList !~ separateByKeyword keyw $ derivs
  return d
chkDataInstance d = return d

separateByKeyword :: DataOrNewtypeKeyword dom -> CheckNode Deriving
separateByKeyword keyw derivs
  | isNewtypeDecl keyw = chkWithByStrat chkClassForNewtype derivs
  | otherwise          = chkWithByStrat chkClassForData    derivs
  where isNewtypeDecl keyw = case keyw ^. element of
                               UNewtypeKeyword -> True
                               _               -> False


getStrategy :: Deriving dom -> Maybe (UDeriveStrategy dom SrcTemplateStage)
getStrategy d
  | Just node <- d ^. deriveStrategy & annMaybe
  , strat <- node ^. element
    = Just strat
  | otherwise = Nothing

addExtension :: (MonadState ExtMap m, HasRange node) =>
                 GHC.Name -> node -> m node
addExtension sname
  | Just ext <- whichExtension sname = addOccurence ext
  | otherwise                        = return

addStockExtension :: CheckNode InstanceHead
addStockExtension x
  | Just sname <- nameFromStock x = addExtension sname x
  | otherwise = return x

chkWithByStrat :: CheckNode' InstanceHead dom -> CheckNode' Deriving dom
chkWithByStrat checker d
  | Just UStockStrategy    <- getStrategy d = chkDerivingClause addStockExtension d
  | Just UNewtypeStrategy  <- getStrategy d = addOccurence GeneralizedNewtypeDeriving d
  | Just UAnyClassStrategy <- getStrategy d = addOccurence DeriveAnyClass d
  | Nothing                <- getStrategy d = chkDerivingClause checker d

chkDerivingClause :: CheckNode' InstanceHead dom -> CheckNode' Deriving dom
chkDerivingClause checker d@(DerivingOne   x)  = checker x >> return d
chkDerivingClause checker d@(DerivingMulti xs) = (annList !~ checker) xs >> return d

-- checks whether the class is stock, and if it is, returns its name
nameFromStock :: HasNameInfo dom => InstanceHead dom -> Maybe GHC.Name
nameFromStock x
  | InstanceHead name <- skipParens x,
    Just sname <- getSemName name,
    isStockClass sname
    = Just sname
  | otherwise = Nothing

chkClassForData :: CheckNode InstanceHead
chkClassForData x
  | Just sname <- nameFromStock x = addExtension sname x
  | otherwise = addOccurence DeriveAnyClass x

-- performs check in case no explicit strategy is given
chkClassForNewtype :: CheckNode InstanceHead
chkClassForNewtype x
  | Just sname <- nameFromStock x
    = if | gndNotNeeded  sname -> return x
         | gndNeeded     sname -> addOccurence GeneralizedNewtypeDeriving x
         | gndNotAllowed sname -> addExtension sname x
  | otherwise = do
      gndOn       <- isTurnedOn GeneralizedNewtypeDeriving
      deriveAnyOn <- isTurnedOn DeriveAnyClass
      if | gndOn && deriveAnyOn -> addOccurence_ DeriveAnyClass x
         | deriveAnyOn          -> addOccurence_ DeriveAnyClass x
         | gndOn                -> addOccurence_ GeneralizedNewtypeDeriving x
         | otherwise             -> return ()
      return x

skipParens :: InstanceHead dom -> InstanceHead dom
skipParens (ParenInstanceHead x) = skipParens x
skipParens x = x

chkStandaloneDeriving :: CheckNode Decl
chkStandaloneDeriving d@(Refact.StandaloneDeriving instRule) = do
  addOccurence_ Ext.StandaloneDeriving d
  let ihead = instRule ^. irHead
      ty    = rightmostType ihead
      cls   = getClassCon   ihead
  itIsNewType    <- isNewtype ty
  itIsSynNewType <- isSynNewType ty
  if itIsNewType || itIsSynNewType
    then chkClassForNewtype cls
    else chkClassForData    cls
  return d
chkStandaloneDeriving d = return d

getClassCon :: InstanceHead dom -> InstanceHead dom
getClassCon (AppInstanceHead f _) = getClassCon f
getClassCon (ParenInstanceHead x) = getClassCon x
getClassCon x = x

rightmostType :: InstanceHead dom -> Type dom
rightmostType ihead
  | AppInstanceHead _ tyvar <- skipParens ihead = tyvar

-- NOTE: Returns false if the type is certainly not a type synonym.
--       Returns true if it is a synonym or it could not have been looked up.
-- This behaviour will produce false positives.
-- This is desirable since the underlying type might be a newtype
-- in which case GeneralizedNewtypeDeriving might be necessary.
isSynNewType :: HasNameInfo dom => Type dom -> ExtMonad Bool
isSynNewType t = do
  mtycon <- runMaybeT . lookupType $ t
  case mtycon of
    Nothing    -> return True
    Just tycon -> isSynNewType' tycon
  where isSynNewType' x = case lookupSynDef x of
                            Nothing  -> return False
                            Just def -> do
                                        addOccurence_ TypeSynonymInstances t
                                        return (GHC.isNewTyCon def)
