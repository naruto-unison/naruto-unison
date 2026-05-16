--  https://github.com/agrafix/elm-bridge/blob/master/src/Elm/Derive.hs
-- with a modified 'compileType' to omit fields that cannot be handled instead
-- of crashing.
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_HADDOCK hide #-}
{-| This module should be used to derive the Elm instance alongside the
 JSON ones. The prefered usage is to convert statements such as :

> $(deriveJSON defaultOptions{fieldLabelModifier = drop 4, constructorTagModifier = map toLower} ''D)

 into:

> $(deriveBoth defaultOptions{fieldLabelModifier = drop 4, constructorTagModifier = map toLower} ''D)

 Which will derive both the @aeson@ and @elm-bridge@ instances at the same
 time.
-}

module ElmDerive
    ( -- * Options
      A.Options(..)
    , A.SumEncoding(..)
    , defaultOptions
    , defaultOptionsDropLower
      -- * Template haskell functions
    , deriveElmDef
    , deriveBoth
    )
where

import Elm.TyRep

import Control.Monad
import Data.Aeson.TH (deriveJSON, SumEncoding(..))
import qualified Data.Aeson.TH as A
import Data.Maybe (mapMaybe)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Char (toLower)
import Control.Applicative
import Prelude

-- | Note that This default set of options is distinct from that in
-- the @aeson@ package.
defaultOptions :: A.Options
defaultOptions
  = A.defaultOptions
  { A.sumEncoding             = A.defaultTaggedObject
  , A.fieldLabelModifier      = id
  , A.constructorTagModifier  = id
  , A.allNullaryToStringTag   = True
  , A.omitNothingFields       = False
  , A.unwrapUnaryRecords      = True
  }

unwrapUnaryRecords :: A.Options -> Bool
unwrapUnaryRecords opts = A.unwrapUnaryRecords opts

{-| This generates a default set of options. The parameter represents the
number of characters that must be dropped from the Haskell field names.
The first letter of the field is then converted to lowercase, ie:

> data Foo = Foo { _fooBarQux :: Int }
> $(deriveBoth (defaultOptionsDropLower 4) ''Foo)

Will be encoded as:

> {"barQux"=12}
-}
defaultOptionsDropLower :: Int -> A.Options
defaultOptionsDropLower n = defaultOptions { A.fieldLabelModifier = lower . drop n }
    where
        lower "" = ""
        lower (x:xs) = toLower x : xs

-- EDITED FROM :: Type -> Q Exp
compileType :: Type -> Maybe (Q Exp)
compileType ty =
    case ty of
      ListT -> Just [|ETyCon (ETCon "List")|]
      TupleT 0 -> Just [|ETyCon (ETCon "()")|]
      TupleT i -> Just [|ETyTuple i|]
      VarT name ->
          let n = nameBase name
          in Just [|ETyVar (ETVar n)|]
      SigT ty' _ ->
          compileType ty'
      AppT a b -> do
          compileA <- compileType a
          compileB <- compileType b
          return [|ETyApp $(compileA) $(compileB)|]
      ConT name ->
          let n = nameBase name
          in Just [|ETyCon (ETCon n)|]
      _ -> Nothing

optSumType :: SumEncoding -> Q Exp
optSumType se =
    case se of
        TwoElemArray -> [|SumEncoding' TwoElemArray|]
        ObjectWithSingleField -> [|SumEncoding' ObjectWithSingleField|]
        TaggedObject tn cn -> [|SumEncoding' (TaggedObject tn cn)|]
        UntaggedValue -> [|SumEncoding' UntaggedValue|]

runDerive :: Name -> [TyVarBndr] -> (Q Exp -> Q Exp) -> Q [Dec]
runDerive name vars mkBody =
    liftM (:[]) elmDefInst
    where
      elmDefInst =
          instanceD (cxt [])
              (classType `appT` instanceType)
              [ funD 'compileElmDef
                         [ clause [ return WildP ] (normalB body) []
                         ]
              ]

      classType = conT ''IsElmDefinition
      instanceType = foldl appT (conT name) $ map varT argNames

      body = mkBody [|ETypeName { et_name = nameStr, et_args = $args }|]

      nameStr = nameBase name
      args =
          listE $ map mkTVar argNames
      mkTVar :: Name -> Q Exp
      mkTVar n =
          let str = nameBase n
          in [|ETVar str|]

      argNames =
          flip map vars $ \v ->
              case v of
                PlainTV tv -> tv
                KindedTV tv _ -> tv

deriveAlias :: Bool -> A.Options -> Name -> [TyVarBndr] -> [VarStrictType] -> Q [Dec]
deriveAlias isNewtype opts name vars conFields =
        runDerive name vars $ \typeName ->
                [|ETypeAlias (EAlias $typeName $fields omitNothing isNewtype unwrapUnary)|] -- default to no newtype
    where
      unwrapUnary = unwrapUnaryRecords opts
      fields = listE $ mapMaybe mkField conFields
      omitNothing = A.omitNothingFields opts
      mkField :: VarStrictType -> Maybe (Q Exp)
      mkField (fname, _, ftype) =
          (<$> compileType ftype) $ \fldType -> [|(fldName, $fldType)|]
        where
          fldName = A.fieldLabelModifier opts $ nameBase fname

deriveSum :: A.Options -> Name -> [TyVarBndr] -> [Con] -> Q [Dec]
deriveSum opts name vars constrs =
    runDerive name vars $ \typeName ->
        [|ETypeSum (ESum $typeName $sumOpts $sumEncOpts omitNothing allNullary)|]
    where
      allNullary = A.allNullaryToStringTag opts
      sumEncOpts = optSumType (A.sumEncoding opts)
      omitNothing = A.omitNothingFields opts
      sumOpts = listE $ map mkOpt constrs
      mkOpt :: Con -> Q Exp
      mkOpt c =
        let modifyName n = (nameBase n, A.constructorTagModifier opts (nameBase n))
        in case c of
            NormalC name' args ->
                let (b, n) = modifyName name'
                    tyArgs = listE $ mapMaybe (\(_, ty) -> compileType ty) args
                in [|STC b n (Anonymous $tyArgs)|]
            RecC name' args ->
                let (b, n) = modifyName name'
                    tyArgs = listE $ mapMaybe (\(nm, _, ty) ->
                        (<$> compileType ty) $ \fldType ->
                        let nm' = A.fieldLabelModifier opts $ nameBase nm
                        in [|(nm', $(fldType))|]) args
                in [|STC b n (Named $tyArgs)|]
            _ -> fail ("Can't derive this sum: " ++ show c)

deriveSynonym :: A.Options -> Name -> [TyVarBndr] -> Type -> Q [Dec]
deriveSynonym _ name vars (compileType -> Just otherType) =
    runDerive name vars $ \typeName ->
        [|ETypePrimAlias (EPrimAlias $typeName $otherType)|]
deriveSynonym _ _ _ _ = return []

-- | Equivalent to running both 'deriveJSON' and 'deriveElmDef' with the
-- same options, so as to ensure the code on the Haskell and Elm size is
-- synchronized.
deriveBoth :: A.Options -> Name -> Q [Dec]
deriveBoth o n = (++) <$> deriveElmDef o n <*> deriveJSON o n

-- | Just derive the @elm-bridge@ definitions for generating the
-- serialization/deserialization code. It must be kept synchronized with
-- the Haskell code manually.
deriveElmDef :: A.Options -> Name -> Q [Dec]
deriveElmDef opts name =
    do TyConI tyCon <- reify name
       case tyCon of
         DataD _ _ tyVars _ constrs _ ->
             case constrs of
               [] -> fail "Can not derive empty data decls"
               [RecC _ conFields] -> deriveAlias False opts name tyVars conFields
               _ -> deriveSum opts name tyVars constrs
         NewtypeD [] _ [] Nothing (NormalC _ [(Bang NoSourceUnpackedness NoSourceStrictness, otherTy)]) [] ->
            deriveSynonym opts name [] otherTy
         NewtypeD [] _ [] Nothing (RecC _ conFields@[(Name (OccName _) _, Bang NoSourceUnpackedness NoSourceStrictness, otherTy)]) [] ->
          if A.unwrapUnaryRecords opts
            then deriveSynonym opts name [] otherTy
            else deriveAlias True opts name [] conFields
         TySynD _ vars otherTy ->
             deriveSynonym opts name vars otherTy
         NewtypeD _ _ tyvars Nothing (NormalC _ [(Bang NoSourceUnpackedness NoSourceStrictness, otherTy)]) [] ->
             deriveSynonym opts name tyvars otherTy
         NewtypeD _ _ tyvars Nothing (RecC _ conFields@[(Name (OccName _) _, Bang NoSourceUnpackedness NoSourceStrictness, otherTy)]) [] ->
          if A.unwrapUnaryRecords opts
            then deriveSynonym opts name tyvars otherTy
            else deriveAlias True opts name tyvars conFields
         _ -> fail ("Oops, can only derive data and newtype, not this: " ++ show tyCon)
