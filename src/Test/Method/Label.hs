{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module : Test.Method.Label
-- Description:
-- License: BSD-3
-- Maintainer: autotaker@gmail.com
-- Stability: experimental
module Test.Method.Label (Label (..), (:|:) (..), deriveLabel) where

import Control.Method (Method (Args, Base))
import Control.Monad.IO.Class (MonadIO)
import Data.Char (isLower, toUpper)
import qualified Data.Kind as K
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Typeable (Typeable)
import Language.Haskell.TH
  ( Dec,
    DecQ,
    DecsQ,
    Name,
    Pred,
    Q,
    TyVarBndr (KindedTV, PlainTV),
    Type (AppT, ArrowT, ConT, ForallT, InfixT, ListT, SigT, TupleT, VarT),
    appE,
    appT,
    caseE,
    conE,
    conP,
    conT,
    cxt,
    dataD,
    gadtC,
    instanceD,
    lam1E,
    match,
    mkName,
    nameBase,
    newName,
    normalB,
    pprint,
    stringE,
    tySynEqn,
    tySynInstD,
    valD,
    varE,
    varP,
    varT,
  )
import qualified Language.Haskell.TH.Datatype as D
import Test.Method.Dynamic (Dynamic, DynamicShow, castMethod)

-- | Type class that represents @f@ denotes the type of field names of @InterfaceOf f@
class Typeable f => Label (f :: K.Type -> K.Type) where
  -- | Interface type corrensponding to @f@
  type InterfaceOf f

  -- | Construct a interface from polymorphic function that returns each field of the interface.
  toInterface ::
    ( forall m.
      ( Typeable m,
        Method m,
        MonadIO (Base m),
        Show (Args m)
      ) =>
      f m ->
      m
    ) ->
    InterfaceOf f

  showLabel :: f m -> String

  compareLabel :: f m1 -> f m2 -> Ordering
  compareLabel x y = showLabel x `compare` showLabel y

-- | @f :|: g@ is the disjoint union of label @f@ and label @g@.
data (:|:) f g a = L (f a) | R (g a)
  deriving (Eq, Ord, Show)

instance (Label f, Label g) => Label (f :|: g) where
  type InterfaceOf (f :|: g) = (InterfaceOf f, InterfaceOf g)
  toInterface k = (f, g)
    where
      f = toInterface (k . L)
      g = toInterface (k . R)
  showLabel (L x) = "L " <> showLabel x
  showLabel (R x) = "R " <> showLabel x

-- |
-- Generate the label type from given interface type.
--
-- * Define GADT @XXXLabel m@ for interface @XXX@.
--
--     * @FieldX :: XXXLabel X@ for each field @fieldX :: X@ where @X@ is a standard type.
--     * @PolyFieldX :: XXXLabel ty[Dynamic/a]@ for each field of the form @polyFieldX :: (forall a. Typeable a => ty)@
--
--         * Type variable @a@ is substituted with @DynamicShow@ if @a@ is instances of 'Show' and 'Typeable'
--         * Type variable @a@ is substituted with @Dynamic@ if @a@ is an instance of 'Typeable' but not 'Show'
--         * Report an error if type variable @a@ is not an instance of 'Typeable'
--
-- * Define instance @Label XXXLabel@.
--
-- ==== Example
--
-- @
-- data API env = API {
--     _foo :: Int -> RIO env Int,
--     _bar :: forall a. (Show a, Typeable a) => String -> RIO env (Maybe a),
--     _baz :: forall b. (Typeable a) => b -> RIO env ()
--   }
-- @
--
-- @deriveLabel ''API@ will generate the following code.
--
-- @
-- data APILabel env m where
--     Foo :: APILabel env (Int -> RIO env Int)
--     Bar :: APILabel env (String -> RIO env (Maybe DynamicShow)) -- type variable \`a\` is replaced with 'DynamicShow'
--     Baz :: APILabel env (Dynamic -> RIO env ()) -- type variable \'b\' is replaced with 'Dynamic'
--
-- instance Label (APILabel env) where
--     type InterfaceOf (APILabel env) = API env
--     toInterface k = API (k Foo) (castMethod (k Bar)) (castMethod (k Baz))
--     showLabel x = case x of
--       Foo -> "Foo"
--       Bar -> "Bar"
--       Baz -> "Baz"
-- @
deriveLabel :: Name -> DecsQ
deriveLabel name = do
  info <- D.reifyDatatype name
  tyVars <- mapM extractTyVar $ D.datatypeInstTypes info
  consInfo <- case D.datatypeCons info of
    [consInfo] -> pure consInfo
    _ -> fail $ "Multiple constructors: " <> pprint name
  fieldNames <- case D.constructorVariant consInfo of
    D.RecordConstructor names -> pure names
    _ -> fail $ "Constructor must be a record: " <> pprint (D.constructorName consInfo)
  labelConNames <- mapM fieldToLabel fieldNames
  let fields = D.constructorFields consInfo
      labelTy = foldl ((. VarT) . AppT) (ConT labelName) tyVarNames
      labelName = mkName $ (++ "Label") $ nameBase name
      tyVarNames = map bndrToName tyVars
      recordTy = D.datatypeType info
      recordConName = D.constructorName consInfo
  labelDec <- deriveLabelData (tyVars, fields, labelTy, labelName, labelConNames)
  labelInstDec <- deriveLabelInst (tyVars, labelTy, recordTy, recordConName, labelConNames)
  pure [labelDec, labelInstDec]

deriveLabelInst :: ([TyVarBndr], Type, Type, Name, [Name]) -> DecQ
deriveLabelInst (tyVars, labelTy, interfaceTy, interfaceConName, labelConNames) =
  instanceD (cxt cxts) (conT ''Label `appT` pure labelTy) [interfaceOfDec, toInterfaceDec, showLabelDec]
  where
    cxts = [conT ''Typeable `appT` varT n | n <- bndrToName <$> tyVars]
    interfaceOfDec = tySynInstD (tySynEqn Nothing (conT ''InterfaceOf `appT` pure labelTy) (pure interfaceTy))
    -- toInterface = \k -> API (castMethod $ k Foo) (castMethod $ k Bar) (castMethod $ k Baz)
    toInterfaceDec = valD (varP 'toInterface) (normalB bodyE) []
      where
        k = mkName "k"
        bodyE = lam1E (varP k) $ foldl step (conE interfaceConName) labelConNames
        step acc labelCon = acc `appE` appE (varE 'castMethod) (appE (varE k) (conE labelCon))
    showLabelDec = valD (varP 'showLabel) (normalB bodyE) []
      where
        x = mkName "x"
        bodyE = lam1E (varP x) (caseE (varE x) (map showCase labelConNames))
        showCase conName =
          match (conP conName []) (normalB $ stringE $ nameBase conName) []

-- |
-- @
-- data API env = API {
--   _foo :: RIO env Int,
--   _bar :: Text -> RIO env Bool,
-- }
-- >>>
-- data APILabel env m where
--   Foo :: APILabel env (RIO env Int)
--   Bar :: APILabel env (Text -> RIO env Bool)
-- @
deriveLabelData :: ([TyVarBndr], [Type], Type, Name, [Name]) -> Q Dec
deriveLabelData (tyVars, fields, labelTy, labelName, labelConNames) = do
  m <- newName "m"
  dataD (cxt []) labelName (tyVars ++ [PlainTV m]) Nothing consQ []
  where
    consQ = zipWith toLabel fields labelConNames
    --  $cName :: $labelName x1 ... xn $fieldTy
    toLabel fieldTy cName = gadtC [cName] [] (pure labelTy `appT` unquantify fieldTy)

unquantify :: Type -> Q Type
unquantify _ty@(ForallT bndrs ctx ty) = do
  tbl <- M.fromList <$> mapM (substBndr _ty ctx) bndrs
  subst tbl ty
unquantify ty = pure ty

substBndr :: Type -> [Pred] -> TyVarBndr -> Q (Name, Type)
substBndr ty preds = go . bndrToName
  where
    go n
      | (ConT ''Typeable `AppT` VarT n) `elem` preds
          && (ConT ''Show `AppT` VarT n) `elem` preds =
        pure (n, ConT ''DynamicShow)
      | (ConT ''Typeable `AppT` VarT n) `elem` preds = pure (n, ConT ''Dynamic)
      | otherwise =
        fail $
          "cannot unquantify: " <> pprint ty
            <> " because Typeable "
            <> pprint n
            <> " constraint is missing"

subst :: M.Map Name Type -> Type -> Q Type
subst tbl (VarT x) = case M.lookup x tbl of
  Just t -> pure t
  Nothing -> pure $ VarT x
subst tbl (AppT f x) = AppT <$> subst tbl f <*> subst tbl x
subst tbl (InfixT x op y) = InfixT <$> subst tbl x <*> pure op <*> subst tbl y
subst _ ArrowT = pure ArrowT
subst _ ty@ConT {} = pure ty
subst _ ty@TupleT {} = pure ty
subst _ ty@ListT = pure ty
subst _ ty@ForallT {} = fail $ "nested forall quantifier is not supported: " <> pprint ty
subst _ ty = fail $ "conversion for " <> pprint ty <> " is not implemented yet. Please raise an issue."

bndrToName :: TyVarBndr -> Name
bndrToName (PlainTV n) = n
bndrToName (KindedTV n _) = n

-- |
-- @
-- fieldToLabel (mkName "_hello") = pure (mkName \"Hello\")
-- @
fieldToLabel :: Name -> Q Name
fieldToLabel fieldName = toConName trimed
  where
    base = nameBase fieldName
    trimed = L.dropWhile (not . isLower) base
    toConName "" = fail $ "cannot convert field name to constructor name: " <> pprint fieldName
    toConName (x : xs) = pure $ mkName $ toUpper x : xs

extractTyVar :: Type -> Q TyVarBndr
extractTyVar (SigT (VarT x) k) = pure $ KindedTV x k
extractTyVar (VarT x) = pure $ PlainTV x
extractTyVar ty = fail $ "cannot extract type variable: " <> pprint ty
