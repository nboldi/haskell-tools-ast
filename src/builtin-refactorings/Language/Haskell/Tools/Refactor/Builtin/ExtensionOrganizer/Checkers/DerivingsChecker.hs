{-# LANGUAGE FlexibleContexts,
             MultiWayIf,
             TypeFamilies
             #-}

{-
  NOTE:
  - DeriveX (X = Data, Generic, Functor, Foldable, Traversable, Lift)
  - same for StandaloneDeriving, but has to check for newtype
  - in case of newtype, we can't really say anything (GeneralizedNtDeriving)
  - DeriveAnyClass (so has to check the "originality" of the class)

  CASES:

  data declaration:
  - is DerivableDefault and original:
    - should do nothing
    - see for details: https://www.haskell.org/onlinereport/derived.html
  - is DerivableExtra and original:
    - add corresponding extension (may still have restrictions)
  - anything else
    - DeriveAnyClass

  newtype declaration:
  - we don't know what the user intended
  - automatically add GeneralizedNewtypeDeriving (it is possible he wanted this)
    - should suggest turning this on (not for Show and Read)
  - however still needs DeriveX for Data and Typeable (and not GNTD)
  - can't generalize nested types
    - ie. they have to be of the form: newtype T v1...vn = T' (t vk+1...vn) deriving (c1...cm)

  StandaloneDeriving:
  - have to lookup type constructor (could be difficult ...)


  TODO:
  - write tests for GADTs, data instances
  - conditional check

  NOTE: Here we implicitly constrained the type with ExtDomain.
        but we only really need HasNameInfo.
-}


module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Checkers.DerivingsChecker where

import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad as Ext
import Control.Reference ((.-), (^.), (!~), (&), biplateRef, Traversal)
import Language.Haskell.Tools.Refactor as Refact hiding (Enum)
import Language.Haskell.Tools.PrettyPrint (prettyPrint)
import Language.Haskell.Tools.AST

import Data.Maybe (listToMaybe, fromMaybe)
import Control.Monad.Trans.Maybe

import SrcLoc (RealSrcSpan, SrcSpan(..))
import qualified Name as GHC (Name)
import qualified GHC
import qualified TyCoRep as GHC
import PrelNames

import Debug.Trace


data DerivableClass = ClsEq
                    | ClsOrd
                    | ClsIx
                    | ClsShow
                    | ClsRead
                    | ClsEnum
                    | ClsBounded
                    | ClsData
                    | ClsTypeable
                    | ClsGeneric
                    | ClsFunctor
                    | ClsFoldable
                    | ClsTraversable
  deriving (Show, Read, Eq, Ord)

whichExtension :: DerivableClass -> Maybe Extension
whichExtension ClsEq          = Nothing
whichExtension ClsOrd         = Nothing
whichExtension ClsIx          = Nothing
whichExtension ClsShow        = Nothing
whichExtension ClsRead        = Nothing
whichExtension ClsEnum        = Nothing
whichExtension ClsBounded     = Nothing
whichExtension ClsData        = Just DeriveDataTypeable
whichExtension ClsTypeable    = Just DeriveDataTypeable
whichExtension ClsGeneric     = Just DeriveGeneric
whichExtension ClsFunctor     = Just DeriveFunctor
whichExtension ClsFoldable    = Just DeriveFoldable
whichExtension ClsTraversable = Just DeriveTraversable

readClass :: String -> Maybe DerivableClass
readClass = fmap fst . listToMaybe . filter (null . snd) . reads . ("Cls" ++)

readIntoExt :: String -> Maybe Extension
readIntoExt s = readClass s >>= whichExtension

wiredInClasses = [ eqClassName
                 , ordClassName
                 , ixClassName
                 , showClassName
                 , readClassName
                 , enumClassName
                 , boundedClassName
                 , dataClassName
                 , typeableClassName
                 , genClassName
                 , functorClassName
                 , foldableClassName
                 , traversableClassName
                 ]

isWiredInClass = flip elem wiredInClasses

chkDerivings = undefined

{-
chkDerivings :: CheckNode Decl
chkDerivings = conditionalAny chkDerivings' derivingExts
           >=> conditional chkStandaloneDeriving Ext.StandaloneDeriving

      where chkDerivings' = chkDataDecl
                        >=> chkGADTDataDecl
                        >=> chkDataInstance

            derivingExts = [ DeriveDataTypeable
                           , DeriveGeneric
                           , DeriveFunctor
                           , DeriveFoldable
                           , DeriveTraversable
                           , DeriveAnyClass
                           , GeneralizedNewtypeDeriving]



chkDataDecl :: CheckNode Decl
chkDataDecl d@(DataDecl keyw _ _ _ derivs) = do
  separateByKeyword keyw derivs
  return d
chkDataDecl d = return d

chkGADTDataDecl :: CheckNode Decl
chkGADTDataDecl d@(GADTDataDecl keyw _ _ _ _ derivs) = do
  addOccurence_ GADTs d
  separateByKeyword keyw derivs
  return d
chkGADTDataDecl d = return d

chkDataInstance :: CheckNode Decl
chkDataInstance d@(DataInstance keyw _ _ derivs) = do
  addOccurence_ TypeFamilies d
  annList !~ (\x -> separateByKeyword keyw x >> return x) $ derivs
  return d
chkDataInstance d = return d

separateByKeyword :: HasNameInfo dom =>
                     DataOrNewtypeKeyword dom ->
                     AnnMaybeG UDeriving dom SrcTemplateStage ->
                     ExtMonad ()
separateByKeyword keyw derivs
  | isNewtypeDecl keyw = checkWith chkClassForNewtype
  | otherwise          = checkWith chkClassForData
  where checkWith f = do
          (annJust !~ chkDerivingClause f) derivs
          return ()
        isNewtypeDecl keyw = case keyw ^. element of
                               UNewtypeKeyword -> True
                               _               -> False



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

-- NOTE: Return false if the type is certainly not a type synonym.
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


-- TODO: could be exported
-- NOTE: Returns Nothing if it is not a type synonym
--       (or has some weird structure I didn't think of)
lookupSynDef :: GHC.TyThing -> Maybe GHC.TyCon
lookupSynDef syn = do
  tycon <- tyconFromTyThing syn
  rhs <- GHC.synTyConRhs_maybe tycon
  tyconFromGHCType rhs

tyconFromTyThing :: GHC.TyThing -> Maybe GHC.TyCon
tyconFromTyThing (GHC.ATyCon tycon) = Just tycon
tyconFromTyThing _ = Nothing

-- won't bother
tyconFromGHCType :: GHC.Type -> Maybe GHC.TyCon
tyconFromGHCType (GHC.AppTy t1 t2) = tyconFromGHCType t1
tyconFromGHCType (GHC.TyConApp tycon _) = Just tycon
tyconFromGHCType _ = Nothing


-- NOTE: Return false if the type is certainly not a newtype
--       Returns true if it is a newtype or it could not have been looked up
isNewtype :: HasNameInfo dom => Type dom -> ExtMonad Bool
isNewtype t = do
  tycon <- runMaybeT . lookupType $ t
  return $! maybe True isNewtypeTyCon tycon

-- TODO: could be exported
lookupType :: HasNameInfo dom => Type dom -> MaybeT ExtMonad GHC.TyThing
lookupType t = do
  name  <- liftMaybe . nameFromType $ t
  sname <- liftMaybe . getSemName   $ name
  MaybeT . GHC.lookupName $ sname
    where liftMaybe = MaybeT . return

-- NOTE: gives just name if the type being scrutinised can be newtype
--       else it gives nothing
nameFromType :: Type dom -> Maybe (Name dom)
nameFromType (TypeApp f _)    = nameFromType f
nameFromType (ParenType x)    = nameFromType x
nameFromType (KindedType t k) = nameFromType t
nameFromType (VarType x)      = Just x
nameFromType _                = Nothing

isNewtypeTyCon :: GHC.TyThing -> Bool
isNewtypeTyCon (GHC.ATyCon tycon) = GHC.isNewTyCon tycon
isNewtypeTyCon _ = False




chkDerivingClause :: HasNameInfo dom =>
                     (InstanceHead dom -> ExtMonad (InstanceHead dom)) ->
                     Deriving dom ->
                     ExtMonad (Deriving dom)
chkDerivingClause checker d@(DerivingOne   x)  = checker x >> return d
chkDerivingClause checker d@(DerivingMulti xs) = do
  let classes = xs ^. annListElems
  mapM_ checker classes
  return d

{-
 Checks an individual class inside a deriving clause in a data declaration
 NOTE: If a class in a deriving clause is wired in, and the code compiles,
       means that the class is a DerivableClass.
       If it is not wired in, or it is not a simple class ctor application,
       then it cannot be derived traditionally.

 NOTE: Works only for "class names". If it gets an input from a standalone
       deriving clause, it has to be simplified.
-}
chkClassForData :: CheckNode InstanceHead
chkClassForData x
  | InstanceHead name <- skipParens x,
    Just sname <- getSemName name,
    isWiredInClass sname
    = do
      let className = name ^. (simpleName & unqualifiedName & simpleNameStr)
      case readIntoExt className of
        Just ext -> addOccurence ext x
        Nothing  -> return x
  | otherwise = addOccurence DeriveAnyClass x


-- TODO: really similar to chkClassForData, try to refactor
-- NOTE: always adds GeneralizedNewtypeDeriving
chkClassForNewtype :: CheckNode InstanceHead
chkClassForNewtype x
  | InstanceHead name <- skipParens x,
    Just sname <- getSemName name,
    isWiredInClass sname
    = do
      let className = name ^. (simpleName & unqualifiedName & simpleNameStr)
      deriveAnyOff <- liftM not $ isTurnedOn DeriveAnyClass
      when (canBeGeneralized sname && deriveAnyOff)
        (addOccurence_ GeneralizedNewtypeDeriving x)
      case readIntoExt className of
        Just ext -> addOccurence ext x
        Nothing  -> return x
  | otherwise = do
      gntdOn      <- isTurnedOn GeneralizedNewtypeDeriving
      deriveAnyOn <- isTurnedOn DeriveAnyClass
      if | gntdOn && deriveAnyOn -> addOccurence_ DeriveAnyClass x
         | deriveAnyOn           -> addOccurence_ DeriveAnyClass x
         | gntdOn                -> addOccurence_ GeneralizedNewtypeDeriving x
         | otherwise             -> return ()
      return x

canBeGeneralized :: GHC.Name -> Bool
canBeGeneralized = not . flip elem notGNTD
notGNTD = [ dataClassName
          , typeableClassName
          , showClassName
          , readClassName]

skipParens :: InstanceHead dom -> InstanceHead dom
skipParens (ParenInstanceHead x) = skipParens x
skipParens x = x

-}

getSemName :: HasNameInfo dom => Name dom -> Maybe GHC.Name
getSemName x = semanticsName (x ^. simpleName)
