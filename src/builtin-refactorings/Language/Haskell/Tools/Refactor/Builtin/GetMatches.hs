module Language.Haskell.Tools.Refactor.Builtin.GetMatches where

import Language.Haskell.Tools.Refactor

import Control.Reference
import Data.Aeson

import Outputable as GHC
import SrcLoc as GHC
import Id as GHC
import Type as GHC
import TyCon as GHC
import DataCon as GHC

getMatchesQuery :: QueryChoice
getMatchesQuery = LocationQuery "GetMatches" getMatches

getMatches :: RealSrcSpan -> ModuleDom -> [ModuleDom] -> QueryMonad QueryValue
getMatches sp (_,mod) _
  = case selectedName of [n] -> fmap (GeneralQuery . toJSON) . getCtors . idType . semanticsId $ n
                         []  -> queryError "No name is selected."
                         _   -> queryError "Multiple names are selected."
  where
    selectedName :: [QualifiedName]
    selectedName = mod ^? nodesContaining sp

getCtors :: GHC.Type -> QueryMonad [(String, [String])]
-- | TODO: unpack forall, context types
-- | TODO: care for infix constructors
getCtors t | Just (tc, _) <- splitTyConApp_maybe t
  = maybe (queryError (noSuccessMsg t)) (return . map formatCtor) (tyConDataCons_maybe tc)
getCtors t = queryError (noSuccessMsg t)

noSuccessMsg :: GHC.Type -> String
noSuccessMsg t = "Cannot find the constructors of type " ++ showSDocUnsafe (ppr t)

formatCtor :: DataCon -> (String, [String])
formatCtor dc = (showSDocUnsafe $ ppr $ dataConName dc, createArgNames (dataConOrigArgTys dc))

-- | TODO: Check for names in scope
-- | TODO: Create names based on the type
createArgNames :: [GHC.Type] -> [String]
createArgNames tys = map (\i -> "p" ++ show i) [1..length tys]
