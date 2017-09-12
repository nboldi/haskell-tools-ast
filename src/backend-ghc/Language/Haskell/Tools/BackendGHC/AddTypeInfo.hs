{-# LANGUAGE TupleSections
           , LambdaCase
           , ScopedTypeVariables
           #-}
module Language.Haskell.Tools.BackendGHC.AddTypeInfo (addTypeInfos) where

import Bag as GHC (bagToList)
import GHC
import HscTypes as GHC
import Id as GHC (Id, mkVanillaGlobal)
import Module as GHC (Module, moduleEnvElts)
import Name as GHC hiding (varName)
import OccName as GHC (OccName, mkDataOcc)
import SrcLoc as GHC
import TcEvidence as GHC (EvBind(..), TcEvBinds(..))
import Type as GHC (Type, mkTyVarTy, mkTyConTy)
import TysWiredIn as GHC (starKindTyCon)
import UniqFM as GHC (eltsUFM)
import UniqSupply as GHC (uniqFromSupply, mkSplitUniqSupply)
import Var as GHC (Var(..))

import Control.Exception
import Control.Applicative (Applicative(..), (<$>), Alternative(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Reference (Lens_1(..), (^.))
import Data.Generics.Uniplate.Data ()
import Data.Generics.Uniplate.Operations (universeBi)
import Data.List as List
import qualified Data.Map as Map (fromList, lookup)
import Data.Maybe (Maybe(..), fromMaybe, catMaybes)

import Language.Haskell.Tools.AST as AST
import Language.Haskell.Tools.AST.SemaInfoTypes as AST (mkCNameInfo)
import Language.Haskell.Tools.BackendGHC.GHCUtils (getTopLevelId)
import Language.Haskell.Tools.BackendGHC.Utils

addTypeInfos :: LHsBinds Id -> Ann AST.UModule (Dom GHC.Name) RangeStage -> Ghc (Ann AST.UModule IdDom RangeStage)
addTypeInfos bnds mod = do
  ut <- liftIO mkUnknownType
  let getType = getType' ut
  fixities <- getFixities
  let createCName sc def id = mkCNameInfo sc def id fixity
        where fixity = if any (any ((getOccName id ==) . getOccName . (^. _1))) (init sc)
                          then Nothing
                          else fmap (snd . snd) $ List.find (\(mod,(occ,_)) -> Just mod == (nameModule_maybe $ varName id) && occ == getOccName id) fixities
  evalStateT (semaTraverse
    (AST.SemaTrf
      (\ni -> case (AST.semanticsSourceInfo ni, AST.semanticsName ni) of
                (_, Just name) -> lift $ createCName (AST.semanticsScope ni) (AST.semanticsDefining ni) <$> getType name
                (Just l@(RealSrcSpan loc), _)
                  -> case Map.lookup l locMapping of
                            Just id -> return $ createCName (AST.semanticsScope ni) (AST.semanticsDefining ni) id
                            _ -> do (none,rest) <- gets (break ((\(RealSrcSpan sp) -> sp `containsSpan` loc) . fst))
                                    case rest of [] -> throw $ ConvertionProblem (RealSrcSpan loc) "Ambiguous or implicit name missing"
                                                 ((_,id):more) -> do put (none ++ more)
                                                                     return $ createCName (AST.semanticsScope ni) (AST.semanticsDefining ni) id
                _ -> convProblem "addTypeInfos: Cannot access a the semantics of a name.")
      pure (traverse (lift . getType)) (traverse (lift . getType)) pure
        pure) mod) (extractSigIds bnds ++ extractSigBindIds bnds)
  where locMapping = Map.fromList $ map (\(L l id) -> (l, id)) $ extractExprIds bnds
        getType' ut name = fromMaybe (mkVanillaGlobal name ut) <$> ((<|> Map.lookup name ids) <$> getTopLevelId name)
        ids = Map.fromList $ map (\id -> (getName id, id)) $ extractTypes bnds
        extractTypes :: LHsBinds Id -> [Id]
        extractTypes = concatMap universeBi . bagToList

        mkUnknownType :: IO Type
        mkUnknownType = do
          tUnique <- mkSplitUniqSupply 'x'
          return $ mkTyVarTy $ mkVanillaGlobal (mkSystemName (uniqFromSupply tUnique) (mkDataOcc "TypeNotFound")) (mkTyConTy starKindTyCon)

        getFixities :: Ghc [(Module, (OccName, GHC.Fixity))]
        getFixities = do env <- getSession
                         pit <- liftIO $ eps_PIT <$> hscEPS env
                         let hpt = hsc_HPT env
                             ifaces = moduleEnvElts pit ++ map hm_iface (eltsUFM hpt)
                         return $ concatMap (\mi -> map (mi_module mi, ) $ mi_fixities mi) ifaces

extractExprIds :: LHsBinds Id -> [Located Id]
        -- expressions like HsRecFld are removed from the typechecked representation, they are replaced by HsVar
extractExprIds = catMaybes . map (\case L l (HsVar (L _ n)) -> Just (L l n)
                                        L l (HsWrap _ (HsVar (L _ n))) -> Just (L l n)
                                        _ -> Nothing
                                 ) . concatMap universeBi . bagToList

extractSigIds :: LHsBinds Id -> [(SrcSpan,Id)]
extractSigIds = concat . map (\case L l bs@(AbsBindsSig {} :: HsBind Id) -> map (l,) $ getImplVars (abs_sig_ev_bind bs)
                                    _                                    -> []
                             ) . concatMap universeBi . bagToList
  where getImplVars (EvBinds evbnds) = catMaybes $ map getEvVar $ bagToList evbnds
        getImplVars _                = []
        getEvVar (EvBind lhs _ False) = Just lhs
        getEvVar _                    = Nothing

extractSigBindIds :: LHsBinds Id -> [(SrcSpan,Id)]
extractSigBindIds = catMaybes . map (\case L l (IPBind (Right id) _) -> Just (l,id)
                                           _                         -> Nothing
                                    ) . concatMap universeBi . bagToList
