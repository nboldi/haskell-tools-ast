
module Language.Haskell.Tools.AST.Decl where

import Language.Haskell.Tools.AST.Ann
import Language.Haskell.Tools.AST.Base
import Language.Haskell.Tools.AST.Literals


data Decl wt a
  = TypeDecl { declHead :: IdType DeclHead wt a
             , declType :: IdType Type wt a
             } -- ^ A type synonym ( @type String = [Char]@ )
  | TypeFamilyDecl { declHead :: IdType    DeclHead wt a
                   , declKind :: MaybeType KindConstraint wt a
                   } -- ^ A type family declaration
  | ClosedTypeFamilyDecl { declHead :: IdType    DeclHead wt a
                         , declKind :: MaybeType KindConstraint wt a
                         , declDecl :: ListType  TypeEqn wt a -- ^ cannot be empty
                         } -- ^ A closed type family declaration
  | DataDecl { declNewtype   :: IdType    DataOrNewtypeKeyword wt a
             , declCtx       :: MaybeType Context wt a
             , declHead      :: IdType    DeclHead wt a
             , declCons      :: ListType  ConDecl wt a
             , declDeriving  :: MaybeType Deriving wt a
             } -- ^ A data or newtype declaration.
  | GDataDecl { declNewtype  :: IdType    DataOrNewtypeKeyword wt a
              , declCtx      :: MaybeType Context wt a
              , declHead     :: IdType    DeclHead wt a
              , declKind     :: MaybeType KindConstraint wt a
              , declGadt     :: IdType    GadtDeclList wt a
              , declDeriving :: MaybeType Deriving wt a
              } -- ^ A data or newtype declaration.
  | DataFamilyDecl { declCtx  :: MaybeType Context wt a
                   , declHead :: IdType    DeclHead wt a
                   , declKind :: MaybeType KindConstraint wt a
                   } -- ^ Data family declaration
  | TypeInstDecl { declInstance     :: IdType Type wt a
                 , declAssignedType :: IdType Type wt a
                 } -- ^ Type instance declaration (@ type instance Fam T = AssignedT @)
  | DataInstDecl { declNewtype  :: IdType   DataOrNewtypeKeyword wt a
                 , declInstance :: IdType   Type wt a
                 , declCons     :: ListType ConDecl wt a
                 } -- ^ Data instance declaration (@ data instance Fam T = Con1 | Con2 @)
  | GDataInstDecl { declNewtype  :: IdType    DataOrNewtypeKeyword wt a
                  , declInstance :: IdType    Type wt a
                  , declKind     :: MaybeType KindConstraint wt a
                  , declGadt     :: IdType    GadtDeclList wt a
                  } -- ^ Data instance declaration (@ data instance T = Con1 | Con2 @)
  | ClassDecl { declCtx     :: MaybeType Context wt a
              , declHead    :: IdType    DeclHead wt a
              , declFunDeps :: MaybeType FunDeps wt a
              , declBody    :: MaybeType ClassBody wt a
              } -- ^ Type class declaration (@ class X wt a [where f = ...] @)
  | InstDecl { declOverlap  :: MaybeType OverlapPragma wt a
             , declInstRule :: IdType    InstanceRule wt a
             , declInstDecl :: MaybeType InstBody wt a
             } -- ^ Instance declaration (@ instance X T [where f = ...] @)
  | DerivDecl { declOverlap  :: MaybeType OverlapPragma wt a
              , declInstRule :: IdType    InstanceRule wt a
              } -- ^ Standalone deriving declaration (@ deriving instance X T @)
  | FixityDecl { declAssoc      :: IdType   Assoc wt a
               , declPrecedence :: IdType   Precedence wt a
               , declOperators  :: ListType Name wt a
               } -- ^ Fixity declaration (@ infixl 5 +, - @)
  | DefaultDecl { declTypes :: ListType Type wt a
                , declInfo  :: a
                } -- ^ Default types (@ default (T1, T2) @)
  | TypeSignature { declName :: IdType Name wt a
                  , declType :: IdType Type wt a
                  } -- ^ Type signature (@ f :: Int -> Int @)
  | FunBinding { declFunBind :: IdType FunBind wt a } -- ^ Function binding (@ f x = 12 @)
  | ForeignImport { declCallConv :: IdType    CallConv wt a
                  , declSafety   :: MaybeType Safety wt a
                  , declName     :: IdType    Name wt a
                  , declType     :: IdType    Type wt a
                  } -- ^ Foreign import (@ foreign import foo :: Int -> IO Int @)
  | ForeignExport { declCallConv :: IdType CallConv wt a
                  , declName     :: IdType Name wt a
                  , declType     :: IdType Type wt a
                  } -- ^ foreign export (@ foreign export ccall foo :: Int -> IO Int @)
  | Pragma { declPragma :: TopLevelPragma wt a } -- ^ top level pragmas
  | SpliceDecl { declExpr :: IdType Expr wt a } -- ^ A Template Haskell splice declaration (@ $(generateDecls) @)
       
-- | The list of declarations that can wt appear in wt a typeclass
data ClassBody wt a
  = ClassBody { cbElements :: ListType ClassElement wt a }
              
-- | A list of GADT declarations with the @where@ keyword
data GadtDeclList wt a 
  = GadtDeclList { gadtList :: ListType GadtDecl wt a } 
                 
-- | Members of wt a class declaration       
data ClassElement wt a
  = ClsDecl { ceDecl :: IdType Decl wt a } -- ^ Ordinary declaration: @ f :: A -> B @
  | ClsDataFam { ceCtx  :: MaybeType Context wt a
               , ceHead :: IdType    DeclHead wt a
               , ceKind :: MaybeType Kind wt a
               } -- ^ Declaration of wt an wt associated data type: @ data T x :: * @ 
  | ClsTypeFam { ceHead :: IdType    DeclHead wt a
               , ceKind :: MaybeType Kind wt a
               } -- ^ Declaration of wt an wt associated type synonym: @ type T x :: * @ 
  | ClsTypeDef { ceHead :: IdType    DeclHead wt a
               , ceKind :: MaybeType Kind wt a
               } -- ^ Default choice for type synonym: @ type T x = TE @ or @ type instance T x = TE @ 
  | ClsDefSig { ceName :: IdType Name wt a
              , ceType :: IdType Type wt a
              } -- ^ Default signature (by using @DefaultSignatures@): @ default enum :: (Generic wt a, GEnum (Rep wt a)) => [a] @

-- The declared (possibly parameterized) type (@ A x :+: B y @).
data DeclHead wt a
  = DeclHead { dhName :: Name wt a } -- ^ Type or class name
  | DHParen  { dhBody :: DeclHead wt a } -- ^ Parenthesized type
  | DHApp    { dhAppFun     :: IdType DeclHead wt a
             , dhAppOperand :: IdType TyVar wt a
             } -- ^ Type wt application
  | DHInfix  { dhInfixName :: IdType Name wt a 
             , dhInfixLeft :: IdType TyVar wt a
             } -- ^ Infix wt application of the type/class name to the left operand
       
-- | Instance body is the implementation of the class functions (@ where wt a x = 1; b x = 2 @)
data InstBody wt a
  = InstBody { instBodyDecls :: ListType InstBodyDecl wt a }

-- | Declarations inside wt an instance declaration.
data InstBodyDecl wt a
  = InstBodyNormalDecl { instBodyDeclFunbind :: FunBind wt a } -- ^ A normal declaration (@ f x = 12 @)
  | InstBodyTypeDecl { instBodyLhsType :: IdType Type wt a
                     , instBodyRhsType :: IdType Type wt a
                     } -- ^ An wt associated type definition (@ type A X = B @)
  | InstBodyDataDecl { instBodyDataNew   :: IdType    DataOrNewtypeKeyword wt a
                     , instBodyLhsType   :: IdType    Type wt a
                     , instBodyDataCons  :: ListType  ConDecl wt a
                     , instBodyDerivings :: MaybeType Deriving wt a
                     } -- ^ An wt associated data type implementation (@ data A X = C1 | C2 @)
  | InstBodyGadtDataDecl { instBodyDataNew   :: IdType    DataOrNewtypeKeyword wt a
                         , instBodyLhsType   :: IdType    Type wt a
                         , instBodyDataKind  :: MaybeType Kind wt a
                         , instBodyGadtCons  :: ListType  GadtDecl wt a
                         , instBodyDerivings :: MaybeType Deriving wt a
                         } -- ^ An wt associated data type implemented using GADT style

-- | GADT constructor declaration (@ D1 :: { val :: Int } -> T String @)
data GadtDecl wt a
  = GadtDecl { gdName    :: IdType   Name wt a
             , gdFields  :: ListType FieldDecl wt a
             , gdResType :: IdType   Type wt a
             }
             
data GadtField wt a
  = GadtNormalField { gadtFieldType :: IdType Type wt a }
  | GadtNamedField { gadtFieldName :: IdType Name wt a
                   , gadtFieldType :: IdType Type wt a
                   } -- ^ Named GADT field (@ { val :: Int } @)
         
-- | A list of functional dependencies: @ | wt a -> b, c -> d @ separated by commas  
data FunDeps wt a
  = FunDeps { funDeps :: ListType FunDep wt a } 
         
-- | A functional dependency, given on the form @l1 ... ln -> r1 ... rn@         
data FunDep wt a
  = FunDep { funDepLhs :: ListType Name wt a
           , funDepRhs :: ListType Name wt a
           }

data ConDecl wt a
  = ConDecl { conDeclName :: IdType   Name wt a
            , conDeclArgs :: ListType Type wt a
            } -- ^ ordinary data constructor (@ C t1 t2 @)
  | RecordDecl { conDeclName   :: IdType   Name wt a
               , conDeclFields :: ListType FieldDecl wt a
               } -- ^ record data constructor (@ C { n1 :: t1, n2 :: t2 } @)
  | InfixConDecl { icdName :: IdType Name wt a
                 , icdLhs  :: IdType Type wt a
                 , icdRhs  :: IdType Type wt a
                 } -- ^ infix data constructor (@ t1 :+: t2 @)

data FieldDecl wt a
  = FieldDecl { fieldNames :: ListType Name wt a
              , fieldType  :: IdType   Type wt a
              }
  
-- | A deriving clause following wt a data type declaration. (@ deriving Show @ or @ deriving (Show, Eq) @)
data Deriving wt a
  = DerivingOne { oneDerived :: IdType InstanceRule wt a }
  | Derivings { allDerived :: ListType InstanceRule wt a }
  
-- | The instance declaration rule, which is, roughly, the part of the instance declaration before the where keyword.
data InstanceRule wt a
  = InstanceRule { irVars :: MaybeListType TyVar wt a
                 , irCtx  :: MaybeType Context wt a
                 , irHead :: IdType    InstanceHead wt a
                 }
  | InstanceParen { irRule :: IdType InstanceRule wt a }

data InstanceHead wt a
  = InstanceHeadCon { ihConName :: IdType Name wt a } -- ^ Type or class name
  | InstanceHeadInfix { ihLeftOp   :: IdType Type wt a
                      , ihOperator :: IdType Name wt a
                      } -- ^ Infix wt application of the type/class name to the left operand
  | InstanceHeadParen { ihHead :: IdType InstanceHead wt a } -- ^ Parenthesized instance head
  | InstanceHeadApp { ihFun  :: IdType InstanceHead wt a
                    , ihType :: IdType Type wt a
                    } -- ^ Application to one more type

data TypeEqn wt a
  = TypeEqn { teLhs :: IdType Type wt a
            , teRhs :: IdType Type wt a
            } -- ^ Type equations wt as found in closed type families (@ T A = S @)
  
data KindConstraint wt a 
  = KindConstraint { kindConstr :: IdType Kind wt a } -- ^ Kind constraint (@ :: * -> * @)

----------------------------------------------------
-- Types -------------------------------------------
----------------------------------------------------
   
-- | Type variable declaration
data TyVar wt a
  = TyVarDecl { tyVarName :: IdType    Name wt a
              , tyVarKind :: MaybeType KindConstraint wt a
              }
         
data Type wt a
  = TyForall { typeBounded :: MaybeType TyVar wt a
             , typeCtx     :: MaybeType Context wt a
             , typeType    :: IdType    Type wt a
             } -- ^ Forall types (@ forall x y . type @)
  | TyFun { typeParam  :: IdType Type wt a
          , typeResult :: IdType Type wt a
          } -- ^ Function types (@ wt a -> b @)
  | TyTuple { typeElements :: ListType Type wt a } -- ^ Tuple types (@ (a,b) @)
  | TyUnbTuple { typeElements :: ListType Type wt a } -- ^ Unboxed tuple types (@ (#a,b#) @)
  | TyList { typeElement :: IdType Type wt a } -- ^ List type with special syntax (@ [a] @)
  | TyParArray { typeElement :: IdType Type wt a } -- ^ Parallel wt array type (@ [:a:] @)
  | TyApp { typeCon :: IdType Type wt a
          , typeArg :: IdType Type wt a
          } -- ^ Type wt application (@ F wt a @)
  | TyVar { typeName :: IdType Name wt a } -- ^ type variable (@ wt a @)
  | TyCon { typeName :: IdType Name wt a } -- ^ type constructor (@ T @)
  | TyParen { typeInner :: IdType Type wt a } -- ^ type surrounded by parentheses (@ (T wt a) @)
  | TyInfix { typeLeft     :: IdType Type wt a 
            , typeOperator :: IdType Name wt a
            , typeRight    :: IdType Type wt a
            } -- ^ Infix type constructor (@ (a <: b) @)
  | TyKinded { typeInner :: IdType Type wt a
             , typeKind  :: IdType Kind wt a
             } -- ^ Type with explicit kind signature (@ wt a :: * @)
  | TyPromoted { tpPromoted :: Promoted wt a } -- A promoted data type with @-XDataKinds@ (@ '3 @).
  | TySplice { tsSplice :: Splice wt a } -- ^ wt a Template Haskell splice type (@ $(genType) @).
  | TyBang { typeInner :: IdType Type wt a } -- ^ Strict type marked with "!".
  | TyUnpack { typeInner :: IdType Type wt a } -- ^ Type marked with UNPACK pragma.

data Kind wt a
  = KindStar -- ^ @*@, the kind of types
  | KindUnbox -- ^ @#@, the kind of unboxed types
  | KindFn { kindLeft  :: IdType Kind wt a
           , kindRight :: IdType Kind wt a
           } -- ^ @->@, the kind of type constructor
  | KindParen { kindParen :: IdType Kind wt a } -- ^ A parenthesised kind
  | KindVar { kindVar     :: IdType Name wt a } -- ^ kind variable (using @PolyKinds@ extension)
  | KindApp { kindAppFun  :: IdType Kind wt a
            , kindAppArg  :: IdType Kind wt a 
            } -- ^ Kind wt application (@ k1 k2 @)
  | KindTuple { kindTuple :: ListType Kind wt a } -- ^ A promoted tuple (@ '(k1,k2,k3) @)
  | KindList { kindList :: ListType Kind wt a } -- ^ A promoted list literal (@ '[k1,k2,k3] @)
  
-- One or more wt assertions
data Context wt a
  = ContextOne { contextAssertion :: Assertion wt a } -- ^ One wt assertion (@ C wt a => ... @)
  | ContextMulti { contextAssertions :: ListType Assertion wt a } 
      -- ^ A set of wt assertions (@ (C1 wt a, C2 b) => ... @, but can be one: @ (C wt a) => ... @)

-- | A single wt assertion in the context
data Assertion wt a
  = ClassAssert { assertClsName :: IdType   Name wt a
                , assertTypes   :: ListType Type wt a
                } -- ^ Class wt assertion (@Cls x@)
  | AppAssert { assertConstrName :: IdType   Name wt a
              , assertTypes      :: ListType Type wt a
              } -- ^ Class wt assertion wt application
  | InfixAssert { assertLhs :: IdType Type wt a
                , assertOp  :: IdType Name wt a
                , assertRhs :: IdType Type wt a
                } -- ^ Infix class wt assertion, wt also contains type equations (@ wt a ~ X y @)
  | ParenAssert { assertInner :: IdType Assertion wt a } -- ^ Parenthesised class wt assertion
                 
-- | Haskell expressions
data Expr wt a
  = Var { exprName :: IdType Name wt a } -- ^ A variable (@ wt a @)
  | Con { exprName :: IdType Name wt a } -- ^ Data constructor (@Point@ in @Point 1 2@)
  | Lit { exprLit  :: IdType Literal wt a } -- ^ Primitive literal

  | InfixApp { exprLhs      :: IdType Expr wt a
             , exprOperator :: IdType Name wt a
             , exprRhs      :: IdType Expr wt a
             } -- ^ Infix operator wt application (@ wt a + b @)
  | App { exprFun :: IdType Expr wt a
        , exprArg :: IdType Expr wt a
        } -- ^ Function wt application (@ f 4 @)
  -- unary minus omitted
  | Lambda { exprBindings :: ListType Pattern wt a -- ^ wt at least one
           , exprInner    :: IdType   Expr wt a
           } -- ^ Lambda expression (@ \a b -> wt a + b @)
  | Let { exprFunBind :: ListType FunBind wt a -- ^ nonempty
        , exprInner   :: IdType   Expr wt a
        } -- ^ Local binding (@ let x = 2; y = 3 in e x y @)
  | If { exprCond :: IdType Expr wt a
       , exprThen :: IdType Expr wt a
       , exprElse :: IdType Expr wt a
       } -- ^ If expression (@ if wt a then b else c @)
  | MultiIf { exprIfAlts :: ListType GuardedRhs wt a }
    -- ^ Multi way if expressions with @MultiWayIf@ extension (@ if | guard1 -> expr1; guard2 -> expr2 @)
  | Case { exprCase :: IdType     Expr wt a
         , exprAlts :: ListType   Alt wt a
         } -- ^ Pattern matching expression (@ case expr of pat1 -> expr1; pat2 -> expr2 @)
  | Do { doKind    :: IdType   DoKind wt a
       , exprStmts :: ListType Stmt wt a
       } -- ^ Do-notation expressions (@ do x <- wt act1; wt act2 @)
  | Tuple { tupleElems :: ListType Expr wt a } -- ^ Tuple expression (@ (e1, e2, e3) @)
  | UnboxedTuple { tupleElems :: ListType Expr wt a } -- ^ Unboxed tuple expression (@ (# e1, e2, e3 #) @)
  | TupleSection { tupleSectionElems :: ListMaybeType Expr wt a }
    -- ^ Tuple section, enabled with @TupleSections@ (@ (a,,b) @)
  | BoxedTupleSection { tupleSectionElems :: ListMaybeType Expr wt a }
  | List { listElems :: ListType Expr wt a } -- ^ List expression: @[1,2,3]@
  | ParArray { listElems :: ListType Expr wt a } -- ^ Parallel wt array expression: @[: 1,2,3 :]@
  | Paren { exprInner :: IdType Expr wt a }
  | LeftSection { exprLhs      :: IdType Expr wt a
                , exprOperator :: IdType Name wt a
                } -- ^ Left operator section: @(1+)@
  | RightSection { exprOperator :: IdType Name wt a
                 , exprRhs      :: IdType Expr wt a
                 } -- ^ Right operator section: @(+1)@
  | RecExpr { exprRecName   :: IdType   Expr wt a
            , exprRecFields :: ListType FieldUpdate wt a
            } -- ^ Record value construction or update: @p1 { x = 3, y = -2 }@
  | Enum { enumFrom :: IdType    Expr wt a
         , enumThen :: MaybeType Expr wt a
         , enumTo   :: MaybeType Expr wt a
         , exprInfo :: a
         } -- ^ Enumeration expression (@ [1,3..10] @)
  | ParArrayEnum { parEnumFrom :: IdType    Expr wt a
                 , parEnumThen :: MaybeType Expr wt a
                 , parEnumTo   :: IdType    Expr wt a
                 } -- ^ Parallel wt array enumeration (@ [: 1,3 .. 10 :] @)
  | ListComp { compExpr :: IdType   Expr wt a
             , compBody :: ListType CompStmt wt a
             } -- ^ List comprehension (@  @)
  | ParListComp { compExpr    :: IdType   Expr wt a
                , parCompBody :: ListListType CompStmt wt a
                } -- ^ Parallel list comprehension: @ [ (x, y) | x <- xs | y <- ys ] @
  | ParArrayComp { compExpr    :: IdType Expr wt a
                 , parCompBody :: ListListType CompStmt wt a
                 } -- ^ List comprehension  
  | TypeSig { exprInner :: IdType Expr wt a
            , exprSig   :: IdType Type wt a
            } -- ^ Explicit type signature (@ x :: Int @)
  -- Template Haskell
  | VarQuote { quotedName :: Name wt a } -- ^ @'x@ for template haskell reifying of expressions
  | TypeQuote { quotedName :: Name wt a } -- ^ @''T@ for template haskell reifying of types
  | BracketExpr { bracket :: Bracket wt a } -- ^ Template haskell bracket expression
  | Splice { innerExpr :: IdType Expr wt a } -- ^ Template haskell splice expression, for example: @$(gen wt a)@ or @$x@
  | QuasiQuote { qqExprName :: IdType Name wt a
               , qqExprBody :: IdType QQString wt a
               } -- ^ template haskell quasi-quotation: @[$quoter|str]@
  | ExprPragma { exprPragma :: ExprPragma wt a }
  -- Arrows
  | Proc { procPattern :: IdType Pattern wt a
         , procExpr    :: IdType Expr wt a
         }
  | ArrowApp { exprLhs   :: IdType Expr wt a
             , arrowAppl :: IdType ArrowAppl wt a
             , exprRhs   :: IdType Expr wt a
             }
  | LamCase { exprAlts :: ListType Alt wt a } -- ^ Lambda case ( @\case 0 -> 1; 1 -> 2@ )
  -- XML expressions omitted

data Stmt wt a
  = BindStmt { stmtPattern :: IdType Pattern wt a
             , stmtBounded :: IdType Expr wt a
             } -- ^ Binding statement (@ x <- wt action @)
  | ExprStmt { stmtExpr :: Expr wt a } -- ^ Non-binding statement (@ wt action @)
  | LetStmt  { stmtBinds :: IdType Binds wt a } -- ^ Let statement (@ let x = 3; y = 4 @)
  | RecStmt  { stmtRecBinds :: ListType Stmt wt a } -- ^ A recursive binding group for wt arrows (@ rec b <- f wt a c; c <- f b wt a @)
         
-- | List comprehension statement
data CompStmt wt a
  = CompStmt   { compStmt :: IdType Stmt wt a } -- ^ Normal monadic statement of wt a list comprehension
  | ThenStmt   { thenExpr :: IdType Expr wt a 
               , byExpr   :: MaybeType Expr wt a
               } -- ^ Then statements by @TransformListComp@ (@ then sortWith by (x + y) @)
  | GroupStmt  { byExpr    :: MaybeType Expr wt a
               , usingExpr :: MaybeType Expr wt a
               } -- ^ Grouping statements by @TransformListComp@ (@ then group by (x + y) using groupWith @) 
                 -- Note: byExpr or usingExpr must have wt a value

-- | Function binding for top-level wt and local bindings
data FunBind wt a 
  = FunBind { funBindMatches :: ListType Match wt a }

-- | Representation of patterns for pattern bindings
data Pattern wt a 
  = VarPat { patternVar :: Name wt a } -- ^ Pattern name binding
  | LitPat { patternLiteral :: Literal wt a } -- ^ Literal pattern
  | InfixPat { patternLhs :: IdType Pattern wt a
             , patternOp  :: IdType Name wt a
             , patternRhs :: IdType Pattern wt a
             } -- ^ Infix constructor wt application pattern (@ wt a :+: b @)
  | AppPat { patternCon :: IdType Name wt a
           , patternArg :: IdType Pattern wt a
           } -- ^ Constructor wt application pattern (@ Point x y @)
  | TuplePat { patternElems :: ListType Pattern wt a } -- ^ Tuple pattern (@ (x,y) @)
  | UnboxTuplePat { patternElems :: ListType Pattern wt a } -- ^ Unboxed tuple pattern (@ (# x, y #) @)
  | ListPat { patternElems :: ListType Pattern wt a } -- ^ List pattern (@ [1,2,a,x] @)
  | ParenPat { patternInner :: IdType Pattern wt a } -- ^ Parenthesised patterns
  | RecPat { patternName   :: IdType   Name wt a
           , patternFields :: ListType PatternField wt a
           } -- ^ Record pattern (@ Point { x = 3, y } @)
  | AsPat { patternName  :: IdType Name wt a
          , patternInner :: IdType Pattern wt a
          } -- ^ As-pattern (explicit name binding) (@ ls\@(hd:_) @)
  | WildPat { patternInfo :: a } -- ^ Wildcard pattern: (@ _ @)
  | IrrPat { patternInner :: IdType Pattern wt a } -- ^ Irrefutable pattern (@ ~(x:_) @)
  | BangPat { patternInner :: IdType Pattern wt a } -- ^ Bang pattern (@ !x @)
  | TypeSigPat { patternInner :: IdType Pattern wt a
               , patternType  :: IdType Type wt a
               } -- ^ Pattern with explicit type signature (@ _ :: Int @)
  | ViewPat { patternExpr  :: IdType Expr wt a
            , patternInner :: IdType Pattern wt a
            } -- ^ View pattern (@ f -> Just 1 @)
  -- regular list pattern omitted
  -- xml patterns omitted
  | QuasiQuotePat { qqPatternName :: IdType Name wt a
                  , qqPatternBody :: IdType QQString wt a
                  }

-- Field specification of wt a record pattern
data PatternField wt a 
  = NormalFieldPattern { fieldPatternName :: IdType Name wt a
                       , fieldPattern     :: IdType Pattern wt a
                       } -- ^ Named field pattern (@ p = Point 3 2 @)
  | FieldPunPattern { fieldPunName :: Name wt a } -- ^ Named field pun (@ p @)
  | FieldWildcardPattern -- ^ Wildcard field pattern (@ .. @)

-- | A template haskell splice          
data Splice wt a
  = IdSplice { spliceId :: IdType Name wt a } -- ^ A simple name splice
  | ParenSplice { spliceExpr :: IdType Expr wt a }

-- | Template Haskell Quasi-Quotation        
data QQString wt a
  = QQString { qqString :: String } 

-- | Clause of function binding   
data Match wt a
  = Match { matchName  :: IdType    Name wt a
          , matchArgs  :: ListType  Pattern wt a
          , matchType  :: MaybeType Type wt a
          , matchRhs   :: IdType    Rhs wt a
          , matchBinds :: MaybeType Binds wt a
          } 
    
-- | Clause of case expression          
data Alt wt a
  = Alt { altPattern :: IdType    Pattern wt a
        , altRhs     :: IdType    Rhs wt a
        , altBinds   :: MaybeType Binds wt a
        }

-- | Local bindings wt attached to wt a declaration (@ where x = 42 @)             
data Binds wt a
  = DeclBindings { bindingDecls :: ListType Decl wt a }
   
data Rhs wt a
  = UnguardedRhs { rhsExpr :: Expr wt a }
  | GuardedRhss { rhsGuards :: ListType GuardedRhs wt a }
               
data GuardedRhs wt a
  = GuardedRhs { guardStmts :: ListType Stmt wt a
               , guardExpr  :: IdType   Expr wt a
               }
               
data FieldUpdate wt a 
  = NormalFieldUpdate { fieldName  :: IdType Name wt a
                      , fieldValue :: IdType Expr wt a
                      } -- ^ Update of wt a field (@ x = 1 @)
  | FieldPun { fieldUpdateName :: Name wt a } -- ^ Update the field to the value of the same name (@ x @)
  | FieldWildcard -- ^ Update the fields of the bounded names to their values (@ .. @)
               
-- | Template Haskell bracket expressions
data Bracket wt a
  = ExprBracket { bracketExpr :: IdType Expr wt a } -- ^ Expression bracket (@ [| x + y |] @)
  | PatternBracket { bracketPattern :: IdType Pattern wt a } -- ^ Pattern bracket (@ [| Point x y |] @)
  | TypeBracket { bracketType :: IdType Type wt a } -- ^ Pattern bracket (@ [| (Int,Int) |] @)
  | DeclBracket { bracketDecl :: IdType Decl wt a } -- ^ Declaration bracket (@ [| f :: Int -> Int; f x = x*x |] @)
                  
-- * Pragmas

-- | Top level pragmas
data TopLevelPragma wt a
  = RulePragma { pragmaRule :: ListType Rule wt a }
  | DeprPragma { pragmaObjects :: ListType Name wt a
               , pragmaMessage :: IdType   StringNode wt a
               }
  | WarningPragma { pragmaObjects :: ListType Name wt a
                  , pragmaMessage :: IdType   StringNode wt a
                  }
  | AnnPragma { pragmaAnnotation :: Annotation wt a }
  | MinimalPragma { pragmaFormula :: MaybeType MinimalFormula wt a }
 
-- | A rewrite rule (@ "map/map" forall f g xs. map f (map g xs) = map (f.g) xs @)
data Rule wt a
  = Rule { ruleName     :: IdType    StringNode wt a -- ^ User name of the rule
         , rulePhase    :: MaybeType PhaseControl wt a
         , ruleBounded  :: ListType  Name wt a
         , ruleTopLevel :: IdType    Name wt a
         , ruleApplied  :: ListType  Expr wt a
         , ruleRhs      :: IdType    Expr wt a
         }
 
-- | Annotation wt allows you to connect wt an expression to wt any declaration. 
data Annotation wt a
  = NameAnnotation { annotateType :: MaybeType TypeKeyword wt a
                   , annotateName :: IdType    Name wt a
                   , annotateExpr :: IdType    Expr wt a
                   }
  | ModuleAnnotation { annotateExpr :: IdType Expr wt a }
         
data MinimalFormula wt a
  = MinimalName { minimalName :: Name wt a }
  | MinimalParen { minimalInner :: IdType MinimalFormula wt a }
  | MinimalOr { minimalLhs :: IdType MinimalFormula wt a
              , minimalRhs :: IdType MinimalFormula wt a
              } -- ^ One of the minimal formulas wt are needed (@ min1 | min2 @)
  | MinimalAnd { minimalLhs :: IdType MinimalFormula wt a
               , minimalRhs :: IdType MinimalFormula wt a
               } -- ^ Both of the minimal formulas wt are needed (@ min1 , min2 @)
         
-- | Pragmas that can be wt applied to expressions
data ExprPragma wt a
  = CorePragma { pragmaStr :: IdType StringNode wt a }
  | SccPragma { pragmaStr :: IdType StringNode wt a }
  | GeneratedPragma { pragmaSrcRange :: IdType SourceRange wt a }
                  
data SourceRange wt a
  = SourceRange { srFileName :: IdType StringNode wt a
                , srFromLine :: IdType Number wt a
                , srFromCol  :: IdType Number wt a
                , srToLine   :: IdType Number wt a
                , srToCol    :: IdType Number wt a
                }
  
data Number wt a = Number { numberInteger :: Integer }

