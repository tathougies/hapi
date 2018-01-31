{-# LANGUAGE PatternGuards #-}

module Main where

import           Web.Hapi.Plugin.Idl
import           Web.Hapi.Types

import           Language.Haskell.Exts.Syntax
import qualified Language.Haskell.Exts.Pretty as Hs

import           Control.Lens hiding (List, to, from)
import qualified Control.Lens as Lens
import           Control.Monad

import           Data.Char
import           Data.List (find)
import           Data.Maybe
import           Data.Monoid ((<>))
import           Data.Proxy
import           Data.Scientific (Scientific)
import qualified Data.Text as T
import           Data.Text.Lens (unpacked, packed)
import           Data.Time (UTCTime)

import           Debug.Trace

import           Network.URI

import           System.Environment

import           GHC.Generics
import           GHC.TypeLits

main :: IO ()
main = do
  [ inputFileName, inputFile, outputFile, idlFile, mode ] <- getArgs

  idl <- parseFile idlFile
  idlData <- lines <$> readFile idlFile

  let hsSyntax
        | mode == "types" = Hs.prettyPrint $ renderHsTypes idl
        | otherwise = Hs.prettyPrint $ renderHsPlugin idl idlData

  if outputFile == "-"
    then putStrLn hsSyntax
    else writeFile outputFile hsSyntax

hsRequestName :: T.Text -> T.Text
hsRequestName = mappend "req_" . view packed .
                identifierHsName . review _Identifier

ident :: String -> Name ()
ident = Ident ()

identPat :: String -> Pat ()
identPat = PVar () . ident

unqual :: String -> QName ()
unqual = UnQual () . ident

varNamed :: String -> Exp ()
varNamed = Var () . unqual

identifierHsName :: Identifier -> String
identifierHsName (Identifier txt) =
  let str = view unpacked txt
  in foldMap (\c -> if c == '-' then "_dash_" else [c]) str

varNamedQ :: String -> String -> Exp ()
varNamedQ mod = Var () . Qual () (ModuleName () mod) . ident

infixOp :: Exp () -> String -> Exp () -> Exp ()
infixOp a op b = InfixApp () a (QVarOp () (UnQual () (Symbol () op))) b

field :: String -> Exp () -> FieldUpdate ()
field nm e = FieldUpdate () (unqual nm) e

text :: T.Text -> Exp ()
text t = Lit () (String () (T.unpack t) (T.unpack t))

string :: String -> Exp ()
string t = Lit () (String () t t)

app :: Exp () -> [ Exp () ] -> Exp ()
app = foldl (App ())

apApp :: Exp () -> [ Exp () ] -> Exp ()
apApp fn [] = app (varNamed "pure") [ fn ]
apApp fn (x:xs) = foldl (\fn' x' -> infixOp fn' "<*>" (Paren () x'))
                        (infixOp (Paren () fn) "<$>" (Paren () x)) xs

tuple :: [ Exp () ] -> Exp ()
tuple = Tuple () Boxed

tyApp :: Type () -> [ Type () ] -> Type ()
tyApp = foldl (TyApp ())

(*->) :: Type () -> Type () -> Type ()
(*->) = TyFun ()
infixr 1 *->

tyCon :: String -> Type ()
tyCon = TyCon () . unqual

tyConQ :: String -> String -> Type ()
tyConQ pkg nm = TyCon () (Qual () (ModuleName () pkg) (ident nm))

tyTuple :: [ Type () ] -> Type ()
tyTuple = TyTuple () Boxed

import_ :: String -> ImportDecl ()
import_ nm = ImportDecl () (ModuleName () nm) False False False Nothing Nothing Nothing

defaultImports :: [ ImportDecl () ]
defaultImports =
  [ import_ "Web.Hapi.Plugin.Interface"
  , import_ "Web.Hapi.Types"
  , import_ "Web.Hapi.Plugin.Idl"
  , import_ "Web.Hapi.Plugin.Monad"
  , import_ "Web.Hapi.Plugin.Interface"
  , (import_ "Data.Text") { importQualified = True, importAs = Just (ModuleName () "T") }
  , (import_ "Data.Monoid") { importSpecs = Just (ImportSpecList () False [IVar () (Symbol () "<>")]) }
  , (import_ "Control.Monad") { importSpecs = Just (ImportSpecList () False [IVar () (ident "guard"), IVar () (ident "when")]) }
  , (import_ "Data.HashMap.Strict") { importQualified = True, importAs = Just (ModuleName () "HM") }
  ]

haskellModuleName :: PluginDescr -> String
haskellModuleName descr =
  case findAttribute "haskellModule" (descr ^. pluginDescrAttrs) ^.
       _Just . _AttributeValueExpression . _ExpressionString . each . _StringPieceText of
    "" -> error "No haskell module name"
    modName -> view unpacked modName

pluginTypeToHs :: PluginDescr -> PluginType -> Type ()
pluginTypeToHs _ (PluginTypeBuiltin BuiltinTypeBool) = tyCon "Bool"
pluginTypeToHs _ (PluginTypeBuiltin BuiltinTypeDecimal) = tyCon "Scientific"
pluginTypeToHs _ (PluginTypeBuiltin BuiltinTypeNumber) = tyCon "Integer"
pluginTypeToHs _ (PluginTypeBuiltin BuiltinTypeString) = tyConQ "T" "Text"
pluginTypeToHs _ (PluginTypeBuiltin BuiltinTypeTimestamp) = tyCon "UTCTime"
pluginTypeToHs _ (PluginTypeBuiltin BuiltinTypeFileUpload) = tyCon "FileUrl"
pluginTypeToHs d (PluginTypeMany a) = TyList () (pluginTypeToHs d a)
pluginTypeToHs d (PluginTypeOptional a) = tyApp (tyCon "Maybe") [ pluginTypeToHs d a ]
pluginTypeToHs d (PluginTypeTuple tys) = tyTuple (map (pluginTypeToHs d) tys)
pluginTypeToHs d (PluginTypeReference (TypeName (Just pkg) nm))
  | pkg == "com.hapi" && nm == "Request" = tyCon "HapiHttpRequest"
  | pkg == "com.hapi" && nm == "Response" = tyCon "HapiHttpResponse"
  | pkg /= d ^. pluginDescrPackage = error "Can't reference external types"
pluginTypeToHs _ (PluginTypeReference (TypeName _ (Identifier nm))) =
  tyCon ("Data'" ++ nm ^. unpacked)

jsonName :: PluginCompoundTypeField -> T.Text
jsonName field =
  case findAttribute "json" (field ^. compoundFieldAttrs) of
    Just (AttributeValueExpression (ExpressionString [StringPieceText nm])) -> nm
    Just {} -> error "Invalid json name"
    _ -> field ^. compoundFieldName . _Identifier

mkJSON :: String -> PluginDataTypeDecl -> [ Decl () ]
mkJSON tyNm (PluginDataTypeData ty) =
  [ InstDecl () Nothing (IRule () Nothing Nothing toJSONInst) (Just [ toJSONDecl ])
  , InstDecl () Nothing (IRule () Nothing Nothing fromJSONInst) (Just [ fromJSONDecl ]) ]
  where
    toJSONInst = IHApp () (IHCon () (unqual "ToJSON")) (tyCon tyNm)
    toJSONDecl = InsDecl () $ FunBind () $
                 [ Match () (ident "toJSON")
                            [ PApp () (unqual tyNm) (map (identPat . ("v" ++) . show . fst)
                                                     (zip [0..] (ty ^.. compoundTypeAllFields))) ]
                            (UnGuardedRhs () toJSONDef) Nothing ]
    toJSONDef =
      app (varNamed "object")
          [ List () $
            flip map (zip [0..] (ty ^.. compoundTypeAllFields)) $ \(i, field) ->
              infixOp (text (jsonName field)) ".="
                      (varNamed ("v" ++ show i)) ]


    fromJSONInst = IHApp () (IHCon () (unqual "FromJSON")) (tyCon tyNm)
    fromJSONDecl = InsDecl () $ FunBind () $
                   [ Match () (ident "parseJSON") [] (UnGuardedRhs () fromJSONDef) Nothing ]
    fromJSONDef =
      app (varNamed "withObject")
          [ string tyNm
          , Lambda () [ identPat "o" ]
                      (apApp (varNamed tyNm) $
                       flip map (ty ^.. compoundTypeAllFields) $ \field ->
                          infixOp (varNamed "o") ".:" (text (jsonName field)))
          ]
mkJSON _ _ = error "mkJSON"

renderHsTypes :: PluginDescr -> Module ()
renderHsTypes descr =
  Module () (Just modHead) modPragmas imports decls
  where
    modHead = ModuleHead () (ModuleName () hsModName) Nothing Nothing
    modPragmas = [ LanguagePragma () [ ident "TupleSections", ident "ScopedTypeVariables", ident "ViewPatterns" ]
                 , OptionsPragma () (Just GHC) "-fno-warn-unused-local-binds -fno-warn-name-shadowing -fno-warn-unused-matches" ]
    hsModName = haskellModuleName descr ++ ".Types"

    imports = defaultImports ++
              [ import_ "Data.Scientific"
              , import_ "Data.Time"
              , import_ "Data.Aeson"
              ]

    decls = do
      (nm, decl) <- descr ^. pluginDescrDecls
      case decl of
        PluginDeclRequest req ->
          mkCompoundTypeDecl (req ^. requestName) ("Args'" ++ req ^. requestName . _Identifier . unpacked) (req ^. requestArgs)
        PluginDeclLookup lookup ->
          let keyNm = "Key'" ++ lookup ^. lookupName . _Identifier . unpacked
              valNm = "Value'" ++ lookup ^. lookupName . _Identifier . unpacked

              keyHapiNm = Identifier (lookup ^. lookupName . _Identifier <> "-key")
              valueHapiNm = Identifier (lookup ^. lookupName . _Identifier <> "-value")
          in mkCompoundTypeDecl keyHapiNm keyNm (lookup ^. lookupKey) ++
             mkCompoundTypeDecl valueHapiNm valNm (lookup ^. lookupValues) ++
             mkLookupDecl nm keyNm valNm
        PluginDeclDataType dataType typeAttrs ->
          let tyNm = "Data'" ++ nm ^. _Identifier . unpacked
          in maybe mempty (\_ -> mkJSON tyNm dataType) (typeAttrs ^? Lens.to (findAttribute "json") . _Just . _AttributeValueOn) ++
             case dataType of
               PluginDataTypeData ty ->
                 mkCompoundTypeDecl nm tyNm ty
               PluginDataTypeOpaque scheme ->
                 mkOpaqueTypeDecl nm tyNm scheme
               PluginDataTypeChoices choices ->
                 mkChoicesTypeDecl nm tyNm choices

    deriving_ = Deriving () [ IRule () Nothing Nothing (IHCon () (unqual "Show"))
                            , IRule () Nothing Nothing (IHCon () (unqual "Eq")) ]

    mkLookupDecl nm keyNm valNm =
      let lookupNm = "lookup_" ++ nm ^. _Identifier . unpacked
          lookupDef = app (varNamed "LookupName") [ text (nm ^. _Identifier) ]
      in [ TypeSig () [ ident lookupNm ] (tyApp (tyCon "LookupName") [tyCon keyNm, tyCon valNm])
         , FunBind () [ Match () (ident lookupNm) [] (UnGuardedRhs () lookupDef) Nothing ] ]

    mkCompoundTypeDecl hapiNm tyNm ty =
      let declHead = DHead () (ident tyNm)
          conDecl = QualConDecl () Nothing Nothing (RecDecl () (ident tyNm) fields)

          fields = do
            field <- ty ^.. compoundTypeAllFields
            pure (FieldDecl () [ident (fieldPrefix ++ "_" ++ field ^. compoundFieldName . _Identifier . unpacked)]
                               (pluginTypeToHs descr (field ^. compoundFieldType)))

          fieldPrefix =
            case tyNm of
              [] -> error "fieldPrefix"
              c:cs -> toLower c:cs

          toHapiValueInst = IHApp () (IHCon () (unqual "ToHapiValue")) (tyCon tyNm)
          toHapiValueDecl = InsDecl () $
                            FunBind () $
                            [ Match () (ident "encodeHapiValue")
                                       [ PApp () (unqual tyNm) (map (identPat . ("v" ++) . show . fst) (zip [0..] (ty ^.. compoundTypeAllFields))) ]
                                       (UnGuardedRhs () toHapiValueDef)
                                       Nothing ]
          toHapiValueDef = app (varNamed "HapiValueObject")
                               [ liftHs (TypeName (Just (descr ^. pluginDescrPackage)) hapiNm)
                               , app (varNamed "HapiObject") . pure . app (varNamedQ "HM" "fromList") . pure .
                                 List () $
                                 map (\(i, field) ->
                                        tuple [ (text (field ^. compoundFieldName . _Identifier))
                                              , app (varNamed "encodeHapiValue") [ varNamed ("v" ++ show i) ] ])
                                     (zip [0..] (ty ^.. compoundTypeAllFields)) ]

          fromHapiObjectInst = IHApp () (IHCon () (unqual "FromHapiObject")) (tyCon tyNm)
          objNmDecl = InsDecl () $ FunBind () $ pure $
                      Match () (ident "fromHapiObjectTypeName") [ PWildCard () ]
                               (UnGuardedRhs () (liftHs $ TypeName (Just (descr ^. pluginDescrPackage)) hapiNm))
                               Nothing

          decodeHapiObjectDecl = InsDecl () $
                                 FunBind () $
                                 [ Match () (ident "decodeHapiObject")
                                            [ PApp () (unqual "HapiObject") [ identPat "obj" ] ]
                                            (UnGuardedRhs () decodeHapiObjectDef) Nothing ]
          decodeHapiObjectDef = apApp (varNamed tyNm)
                                      (map decodeField (ty ^.. compoundTypeAllFields))

          decodeField field =
            let res' = Do () $
                  [ Generator () (PApp () (unqual "Just") [identPat "x"])
                                  (app (varNamed "pure") [ app (varNamedQ "HM" "lookup") [ text (field ^. compoundFieldName . _Identifier)
                                                                                         , varNamed "obj" ] ])
                  , Qualifier () (app (varNamed "pure") [varNamed "x"]) ]

                makeValue ty res =
                  case ty of
                    PluginTypeBuiltin BuiltinTypeBool ->
                      [ Generator () (PApp () (unqual "HapiValueSwitch") [ identPat "v"]) res ]
                    PluginTypeBuiltin BuiltinTypeDecimal ->
                      [ Generator () (PApp () (unqual "HapiValueNumber") [ identPat "v" ]) res ]
                    PluginTypeBuiltin BuiltinTypeNumber ->
                      [ Generator () (PApp () (unqual "HapiValueNumber") [ PViewPat () (varNamed "floor") (identPat "v") ]) res ]
                    PluginTypeBuiltin BuiltinTypeString ->
                      [ Generator () (PApp () (unqual "HapiValueText") [ identPat "v" ]) res ]
                    PluginTypeBuiltin BuiltinTypeFileUpload ->
                      [ Generator () (PApp () (unqual "HapiValueFile") [ PViewPat () (varNamed "FileUrl") (identPat "v") ]) res ]
                    PluginTypeBuiltin BuiltinTypeTimestamp ->
                      [ Generator () (PApp () (unqual "HapiValueTimestamp") [ identPat "v" ]) res ]
                    PluginTypeOptional ty ->
                      [ Generator () (identPat "v'") res
                      , Generator () (identPat "v") $
                        Case () (varNamed "v'") $
                        [ Alt () (PApp () (unqual "HapiValueNothing") []) (UnGuardedRhs ()(app (varNamed "pure") [ varNamed "Nothing" ])) Nothing
                        , Alt () (PApp () (unqual "HapiValueJust") [identPat "x"])
                                 (UnGuardedRhs () $
                                  Do () (makeValue ty (app (varNamed "pure") [ varNamed "x" ]) ++
                                         [Qualifier () $ app (varNamed "pure") [ app (varNamed "Just") [ varNamed "v" ] ]])) Nothing
                        , Alt () (PWildCard ()) (UnGuardedRhs () (app (varNamed "fail") [ string "optional" ])) Nothing ] ]
                    PluginTypeReference ty@(TypeName (Just pkgName) _)
                      | pkgName == "com.hapi" -> [ Generator () (PApp () (unqual "HapiValueObject") [ identPat "ty", identPat "obj" ]) res
                                                 , Qualifier () (app (varNamed "guard") [infixOp (liftHs ty) "==" (varNamed "ty")])
                                                 , Generator () (identPat "v") (app (varNamed "decodeHapiObject") [ varNamed "obj" ]) ]
                      | pkgName /= descr ^. pluginDescrPackage -> [ Generator () (identPat "v") (app (varNamed "fail") [ string  "reference makevalue" ]) ]
                    PluginTypeReference (TypeName _ tyName) ->
                      let expTy = TypeName (Just (descr ^. pluginDescrPackage)) tyName
                      in [ Generator () (PApp () (unqual "HapiValueObject") [ identPat "tyNm", identPat "obj" ]) res
                         , Qualifier () (app (varNamed "when") [ infixOp (varNamed "tyNm") "/=" (liftHs expTy)
                                                               , app (varNamed "fail") [ app (varNamed "mconcat") . pure . List () $
                                                                                         [ string $ "Couldn't match expected type " ++ show expTy ++ " with "
                                                                                         , app (varNamed "show") [ varNamed "tyNm" ]] ]])
                         , Generator () (identPat "v") (app (varNamed "decodeHapiObject") [ varNamed "obj" ]) ]
                    PluginTypeMany ty ->
                      [ Generator () (PApp () (unqual "HapiValueList") [ identPat "xs" ]) res
                      , Generator () (identPat "v") (app (varNamed "mapM") [Lambda () [identPat "v"] (Do () (makeValue ty (app (varNamed "pure") [ varNamed "v" ]) ++
                                                                                                            [ Qualifier () $ app (varNamed "pure") [varNamed "v"] ]))
                                                                           , varNamed "xs" ]) ]
                    _ -> error ("makeValue" ++ show ty)

            in Do () ( makeValue (field ^. compoundFieldType) res' ++
                       [ Qualifier () (app (varNamed "pure") [ varNamed "v" ]) ] )

      in [ DataDecl () (DataType ()) Nothing declHead [ conDecl ] (Just deriving_)
         , InstDecl () Nothing (IRule () Nothing Nothing toHapiValueInst) (Just [toHapiValueDecl])
         , InstDecl () Nothing (IRule () Nothing Nothing fromHapiObjectInst) (Just [decodeHapiObjectDecl, objNmDecl]) ]
    mkOpaqueTypeDecl hapiNm tyNm _ =
      let conDecl = QualConDecl () Nothing Nothing (ConDecl () (ident tyNm) [tyConQ "T" "Text"])
          toHapiValueInst = IHApp () (IHCon () (unqual "ToHapiValue")) (tyCon tyNm)
          toHapiValueDecl = InsDecl () $
                            FunBind () $
                            [ Match () (ident "encodeHapiValue")
                                       [ PApp () (unqual tyNm) [ identPat "payload" ] ]
                                       (UnGuardedRhs () toHapiValueDef) Nothing ]
          toHapiValueDef = app (varNamed "HapiValueObject")
                               [ liftHs (TypeName (Just (descr ^. pluginDescrPackage)) hapiNm)
                               , app (varNamed "HapiObject")
                                     [ app (varNamedQ "HM" "singleton")
                                           [ string "payload"
                                           , app (varNamed "encodeHapiValue") [ varNamed "payload" ] ] ] ]

          fromHapiObjectInst = IHApp () (IHCon () (unqual "FromHapiObject")) (tyCon tyNm)
          objNmDecl = InsDecl () $ FunBind () $ pure $
                      Match () (ident "fromHapiObjectTypeName") [ PWildCard () ]
                            (UnGuardedRhs () (liftHs (TypeName (Just (descr ^. pluginDescrPackage)) hapiNm))) Nothing
          fromHapiObjectDecl = InsDecl () $ FunBind () $
                               [ Match () (ident "decodeHapiObject")
                                          [ PApp () (unqual "HapiObject") [ identPat "obj" ] ]
                                          (UnGuardedRhs () fromHapiObjectDef) Nothing ]
          fromHapiObjectDef =
            apApp (varNamed tyNm) . pure $
            Do () [ Generator () (PApp () (unqual "Just") [PApp () (unqual "HapiValueText") [identPat "v"]])
                                 (app (varNamed "pure") [app (varNamedQ "HM" "lookup") [ string "payload", varNamed "obj" ]])
                  , Qualifier () (app (varNamed "pure") [varNamed "v"]) ]

      in [ DataDecl () (NewType ()) Nothing (DHead () (ident tyNm)) [ conDecl ] (Just deriving_)
         , InstDecl () Nothing (IRule () Nothing Nothing toHapiValueInst) (Just [toHapiValueDecl])
         , InstDecl () Nothing (IRule () Nothing Nothing fromHapiObjectInst) (Just [fromHapiObjectDecl, objNmDecl]) ]
    mkChoicesTypeDecl hapiNm tyNm choices =
      let conDecls = do
            PluginDataTypeChoice conNm payload <- choices
            pure (QualConDecl () Nothing Nothing
                   (ConDecl () (ident (tyNm ++ "_" ++ conNm ^. _Identifier . unpacked))
                    (case payload of
                       Nothing -> []
                       Just ty -> [ pluginTypeToHs descr (PluginTypeReference ty) ])))

          toHapiValueInst = IHApp () (IHCon () (unqual "ToHapiValue")) (tyCon tyNm)
          toHapiValueDecl = InsDecl () $ FunBind () toHapiValueDecls
          toHapiValueDecls = do
            PluginDataTypeChoice choiceNm payload <- choices
            let conNm = tyNm ++ "_" ++ (choiceNm ^. _Identifier . unpacked)
                encodePre x = app (varNamed "HapiValueObject")
                                  [ liftHs (TypeName (Just (descr ^. pluginDescrPackage)) hapiNm)
                                  , app (varNamed "HapiObject") [ app (varNamedQ "HM" "fromList") [ List () x ] ] ]
                encodeDef = encodePre $
                            case payload of
                              Nothing -> [ tuple [ string "type", app (varNamed "HapiValueText") [ text (choiceNm ^. _Identifier)] ]]
                              Just {} -> [ tuple [ string "type", app (varNamed "HapiValueText") [ text (choiceNm ^. _Identifier)] ]
                                         , tuple [ string "payload", app (varNamed "encodeHapiValue") [ varNamed "payload" ] ] ]
            pure $ Match () (ident "encodeHapiValue")
                            [ PApp () (unqual conNm) $
                              case payload of
                                Nothing -> []
                                Just {} -> [ identPat "payload" ] ]
                            (UnGuardedRhs () encodeDef) Nothing

          fromHapiObjectInst = IHApp () (IHCon () (unqual "FromHapiObject")) (tyCon tyNm)
          objNmDecl = InsDecl () $ FunBind () $ pure $
                      Match () (ident "fromHapiObjectTypeName") [ PWildCard () ]
                            (UnGuardedRhs () (liftHs (TypeName (Just (descr ^. pluginDescrPackage)) hapiNm))) Nothing
          fromHapiObjectDecl = InsDecl () $ FunBind () $
                               [ Match () (ident "decodeHapiObject")
                                          [ PApp () (unqual "HapiObject") [ identPat "obj" ] ]
                                          (UnGuardedRhs () fromHapiObjectDef) Nothing ]
          fromHapiObjectDef =
            Do () [ Generator () (PApp () (unqual "Just") [PApp () (unqual "HapiValueText") [identPat "conNm"]])
                                 (app (varNamed "pure") [app (varNamedQ "HM" "lookup") [ string "type", varNamed "obj" ]])
                  , Qualifier () $ Case () (varNamed "conNm") $
                    (flip map choices $ \choice ->
                        Alt () (PLit () (Signless ()) (String () (choice ^. dataTypeChoiceConName . _Identifier . unpacked) (choice ^. dataTypeChoiceConName . _Identifier . unpacked)))
                        (UnGuardedRhs () $
                            case choice ^. dataTypeChoiceData of
                              Nothing -> app (varNamed "pure") [varNamed (tyNm ++ "_" ++ choice ^. dataTypeChoiceConName . _Identifier . unpacked)]
                              Just ty -> error "Payload in choices") Nothing) ++
                    [ Alt () (PWildCard ()) (UnGuardedRhs () $ app (varNamed "fail") [ string "No such constructor" ]) Nothing ]
                  ]
      in [ DataDecl () (DataType ()) Nothing (DHead () (ident tyNm)) conDecls (Just deriving_)
         , InstDecl () Nothing (IRule () Nothing Nothing toHapiValueInst) (Just [toHapiValueDecl])
         , InstDecl () Nothing (IRule () Nothing Nothing fromHapiObjectInst) (Just [fromHapiObjectDecl, objNmDecl]) ]

renderHsPlugin :: PluginDescr -> [String] -> Module ()
renderHsPlugin descr idlData =
  Module () (Just modHead) modPragmas imports
         ( [ mainSig, mainFunc, idlDataSig, idlDataDecl, apiDescrSig, apiDataDecl

           , normalizeSig, normalizeDecl

           , fillSig, fillDecl

           , formSig, formDecl

           , dispatchSig, dispatchDecl ] )
  where
    modHead = ModuleHead () (ModuleName () "Main") Nothing Nothing
    modPragmas = [ LanguagePragma () [ ident "TupleSections", ident "ScopedTypeVariables" ]
                 , OptionsPragma () (Just GHC) "-fno-warn-unused-local-binds -fno-warn-name-shadowing -fno-warn-unused-matches"]
    hsModName = haskellModuleName descr
    imports = defaultImports ++
              [ import_ (hsModName <> ".Types")
              , (import_ hsModName)
                { importSpecs = Just (ImportSpecList () False (IVar () . Ident () <$> implementedSymbols))
                , importQualified = True, importAs = Just (ModuleName () "Impl") } ]

    implementedSymbols = do
      (_, PluginDeclRequest req) <- descr ^. pluginDescrDecls
      case findAttribute "unimplemented" (req ^. requestAttributes) of
        Just AttributeValueOn -> mzero
        _ -> pure (view unpacked (hsRequestName (req ^. requestName . _Identifier)))

    mainFunc = FunBind () [ Match () (ident "main") [] (UnGuardedRhs () mainDef) Nothing]
    mainDef = app (varNamed "pluginMain")
                  [ RecConstr () (unqual "Plugin")
                                 [ field "_pluginName" (text (descr ^. pluginDescrPackage . _PackageName))
                                 , field "_pluginIdl" (varNamed "idlData")
                                 , field "_pluginDescription" (varNamed "apiDescr")
                                 , field "_pluginConfigTemplate" compiledConfigTemplate
                                 , field "_pluginConfigTypeName" (liftHs configTypeName)
                                 , field "_pluginConfigureRequest" (liftHs configRequestName)
                                 , field "_pluginNormalize" (varNamed "normalize")
                                 , field "_pluginFill" (varNamed "fill")
                                 , field "_pluginForm" (varNamed "mkForm")
                                 , field "_pluginImpl" (varNamed "dispatch") ] ]

    idlDataDecl = FunBind () [ Match () (ident "idlData") [] (UnGuardedRhs () idlDataDef) Nothing ]
    idlDataDef = app (varNamedQ "T" "unlines") [ List () (map string idlData) ]

    mainSig = TypeSig () [ ident "main" ] (tyApp (tyCon "IO") [ tyTuple [] ])
    idlDataSig = TypeSig () [ ident "idlData" ] (tyConQ "T" "Text")
    apiDescrSig = TypeSig () [ ident "apiDescr" ] (tyCon "ApiDescription")

    apiDataDecl = FunBind () [ Match () (ident "apiDescr") [] (UnGuardedRhs () (liftHs compiled)) Nothing ]

    compiled = compileIdl descr
    compiledConfigTemplate = compileConfigType (descr ^. pluginDescrPackage) configTypeDef

    normalizeSig = TypeSig () [ ident "normalize" ] (tyCon "TypeName" *-> tyCon "HapiObject" *-> tyApp (tyCon "PluginM") [ tyCon "HapiObject" ])
    normalizeDecl = FunBind () (do { (ident, decl) <- descr ^. pluginDescrDecls
                                   ; case decl of
                                       PluginDeclDataType (PluginDataTypeData ct) _ -> pure (mkNormalize ident ct)
                                       PluginDeclRequest r -> pure (mkNormalize ident (r ^. requestArgs))
                                       _ -> mzero } ++
                                [ Match () (ident "normalize")
                                           [ PApp () (unqual "TypeName") [ PApp () (unqual "Nothing") [], PWildCard () ], PWildCard () ]
                                           (UnGuardedRhs () normalizeErrorDef) Nothing
                                , Match () (ident "normalize") normalizeExternalArgs (GuardedRhss () [ GuardedRhs () normalizeUnqualGuards normalizeUnqualDef ]) Nothing
                                , Match () (ident "normalize") normalizeExternalArgs (UnGuardedRhs () normalizeExternalDef) Nothing ])

    normalizeUnqualGuards = [ Qualifier () (infixOp (varNamed "tyPkg") "==" (text (descr ^. pluginDescrPackage . _PackageName))) ]
    normalizeUnqualDef = app (varNamed "normalize") [ app (varNamed "TypeName") [ varNamed "Nothing", varNamed "tyName" ], varNamed "obj" ]

    normalizeExternalArgs = [ PApp () (unqual "TypeName") [ PApp () (unqual "Just") [ identPat "tyPkg" ], identPat "tyName" ], identPat "obj" ]
    normalizeExternalDef = app (varNamed "normalizeExternalType") [ app (varNamed "TypeName") [ app (varNamed "Just") [ varNamed "tyPkg" ], varNamed "tyName" ]
                                                                  , varNamed "obj" ]

    normalizeInternalArgs identifier = [ PApp () (unqual "TypeName") [ PApp () (unqual "Nothing") [],  PLit () (Signless ()) (String () identifier identifier) ], identPat "obj" ]

    mkNormalize identifier ct = Match () (ident "normalize") (normalizeInternalArgs (identifier ^. _Identifier . unpacked))
                                         (UnGuardedRhs () (app (varNamed "doNormalize") [ liftHs ct, varNamed "obj" ]))
                                         Nothing
    normalizeErrorDef = app (varNamed "fail") [ string "Type name not found" ]

    (configRequestName, configTypeName, configTypeDef) =
      case configurationTypes of
        [] -> error "No configuration type"
        [x] -> x
        _ -> error "Too many configuration types"

    configurationTypes = do
        (reqNm, PluginDeclRequest request) <- descr ^. pluginDescrDecls
        traceM ("Request " ++ show (request ^. requestAttributes))
        Just AttributeValueOn <- pure (findAttribute "configure" (request ^. requestAttributes))
        traceM ("Args " ++ show (request ^. requestArgs))
        TypeName Nothing tyName <- request ^.. requestArgs . compoundTypeFields . each . filtered (\f -> f ^. compoundFieldName == "new") . compoundFieldType . _PluginTypeReference

        traceM ("Type " ++ show tyName)

        (tyName', PluginDeclDataType (PluginDataTypeData ty) _) <- descr ^. pluginDescrDecls
        guard (tyName' == tyName)

        pure (reqNm, tyName, ty)

    fillSig = TypeSig () [ ident "fill" ] (tyCon "Identifier" *-> tyCon "HapiObject" *-> tyApp (tyCon "PluginM") [ tyCon "HapiObject" ])
    fillDecl = FunBind () (do { (declNm, decl) <- descr ^. pluginDescrDecls
                              ; case decl of
                                  PluginDeclDataType (PluginDataTypeData ct) _ -> pure (mkFill (descr ^. pluginDescrPackage) declNm ct)
                                  PluginDeclDataType (PluginDataTypeChoices (x:_)) _ ->
                                    let con = varNamed ("Data'" ++ declNm ^. _Identifier . unpacked ++ "_" ++ x ^. dataTypeChoiceConName . _Identifier . unpacked)
                                        body = Do () [ Generator () (PApp () (unqual "HapiValueObject") [ PWildCard (), identPat "v" ]) (app (varNamed "pure") [ app (varNamed "encodeHapiValue") [ con ] ])
                                                     , Qualifier () (app (varNamed "pure") [ varNamed "v" ]) ]
                                        identifierTxt = declNm ^. _Identifier . unpacked
                                    in [ Match () (ident "fill") [ PLit () (Signless ()) (String () identifierTxt identifierTxt), PWildCard () ] (UnGuardedRhs () body) Nothing ]
                                  PluginDeclRequest r -> pure (mkFill (descr ^. pluginDescrPackage) declNm (r ^. requestArgs))
                                  _ -> mzero } ++
                           [Match () (ident "fill") [ PWildCard (), PWildCard () ] (UnGuardedRhs () fillErrorDef) Nothing])
    fillErrorDef = app (varNamed "error") [ string "fill" ]

    formSig = TypeSig () [ ident "mkForm" ] (tyCon "Identifier" *-> tyCon "HapiObject" *-> tyApp (tyCon "PluginM") [ tyApp (tyCon "Either") [ tyCon "ApiSettingsControl", tyCon "ApiSettingsDescription" ] ])
    formDecl = FunBind () (do { (ident, decl) <- descr ^. pluginDescrDecls
                              ; case decl of
                                  PluginDeclDataType (PluginDataTypeData ct) _ -> pure (mkForm (descr ^. pluginDescrPackage) ident ct)
                                  PluginDeclDataType (PluginDataTypeChoices choices) _ -> mkChoicesControl ident choices
                                  PluginDeclRequest r -> pure (mkForm (descr ^. pluginDescrPackage) ident (r ^. requestArgs))
                                  _ -> mzero } ++
                           [Match () (ident "mkForm") [ identPat "nm", PWildCard () ] (UnGuardedRhs () formErrorDef) Nothing])
    formErrorDef = app (varNamed "fail") [ infixOp (string "no form for ") "++" (app (varNamed "show") [varNamed "nm"]) ]

    dispatchSig = TypeSig () [ ident "dispatch" ] (tyCon "Identifier" *-> tyCon "HapiObject" *-> tyApp (tyCon "PluginM") [ tyTuple [tyCon "PluginType", tyCon "HapiValue"] ])
    dispatchDecl = FunBind () (do { (_, PluginDeclRequest decl) <- descr ^. pluginDescrDecls
                                  ; case findAttribute "unimplemented" (decl ^. requestAttributes) of
                                      Just AttributeValueOn -> mzero
                                      _ -> pure (mkDispatch descr decl) } ++
                               [ Match () (ident "dispatch") [ PWildCard (), PWildCard () ] (UnGuardedRhs () dispatchErrorDef) Nothing ])
    dispatchErrorDef = app (varNamed "error") [ string "dispatch: unimplemented" ]

labeledCompoundFields :: PluginCompoundType -> [(String, PluginCompoundTypeField)]
labeledCompoundFields = zip (map (('v':) . show) [0..]) . toListOf compoundTypeAllFields

addVarToCtxt :: Maybe (Exp ()) -> String -> PluginCompoundTypeField -> Maybe (Exp ())
addVarToCtxt Nothing  vn field = Just $ app (varNamed "varCtxt") [ text (field ^. compoundFieldName . _Identifier), varNamed vn ]
addVarToCtxt (Just e) vn field = Just $ infixOp (app (varNamed "varCtxt") [ text (field ^. compoundFieldName . _Identifier), varNamed vn ]) "<>" e

mkVarCtxt :: Maybe (Exp ()) -> Exp ()
mkVarCtxt (Just e) = e
mkVarCtxt Nothing = varNamed "mempty"

mkDispatch :: PluginDescr -> PluginRequestDecl -> Match ()
mkDispatch descr decl =
  Match () (ident "dispatch") [ PLit () (Signless ()) (String () identifierTxt identifierTxt)
                              , identPat "args" ]
           (UnGuardedRhs () body) Nothing
  where
    identifierTxt = decl ^. requestName . _Identifier . unpacked

    body = Do () [ Generator () (identPat "decodedArgs") (app (varNamed "parseInPlugin") [app (varNamed "decodeHapiObject") [ varNamed "args" ]])
                 , Generator () (PatTypeSig () (identPat "result") (pluginTypeToHs descr (decl ^. requestResultType)))
                                      (app (varNamedQ "Impl" (decl ^. requestName . _Identifier . Lens.to hsRequestName . unpacked))
                                             [varNamed "decodedArgs"])
                 , Generator () (identPat "result'") (app (varNamed "pure") [ app (varNamed "encodeHapiValue") [ varNamed "result" ] ])
                 , Qualifier () (app (varNamed "pure") [ tuple [ liftHs (decl ^. requestResultType)
                                                               , varNamed "result'" ] ])]

mkFill :: PackageName -> Identifier -> PluginCompoundType -> Match ()
mkFill pkgNm identifier ty =
  Match () (ident "fill") [ PLit () (Signless ()) (String () identifierTxt identifierTxt)
                          , PApp () (unqual "HapiObject") [ identPat "obj" ] ]
           (UnGuardedRhs () body) Nothing
  where
    identifierTxt = identifier ^. _Identifier. unpacked
    body = Do () (snd (foldl fillField (Nothing, id) allFields)
                  [ Qualifier () (app (varNamed "pure") [ app (varNamed "HapiObject") [ app (varNamedQ "HM" "fromList") [ List () returnFields ]]]) ])

    allFields = labeledCompoundFields ty
    returnFields = map (\(vn, field) -> tuple [ text (field ^. compoundFieldName . _Identifier), varNamed vn]) allFields

    fillField (vars, a) (vn, field) =
      let e = Case () (app (varNamedQ "HM" "lookup") [ text (field ^. compoundFieldName . _Identifier), varNamed "obj" ])
                      [ Alt () (PApp () (unqual "Just") [ identPat "x" ]) (UnGuardedRhs () (app (varNamed "pure") [varNamed "x"])) Nothing
                      , Alt () (PApp () (unqual "Nothing") []) (UnGuardedRhs () fillFieldExp) Nothing ]

          fillFieldExp =
            case findAttribute "fill" (field ^. compoundFieldAttrs) of
              Just (AttributeValueExpression e) ->
                app (varNamed "runIdlExpression") [ mkVarCtxt vars, liftHs e ]
              _ | Just AttributeValueOn <- findAttribute "config" (field ^. compoundFieldAttrs) ->
                  varNamed "getPluginConfig"
                | otherwise ->
                  case field ^. compoundFieldType of
                    PluginTypeReference (TypeName Nothing localNm) ->
                      apApp (app (varNamed "HapiValueObject") [ liftHs (TypeName (Just pkgNm) localNm) ])
                            [ app (varNamed "fill") [ liftHs localNm, app (varNamed "HapiObject") [ varNamed "mempty" ] ] ]
                    fieldTy ->
                      app (varNamed "mkHapiConfigDefault") [ liftHs fieldTy ]
      in ( addVarToCtxt vars vn field
         , a . (Generator () (identPat vn) e:) )

mkChoicesControl :: Identifier -> [ PluginDataTypeChoice ] -> [ Match () ]
mkChoicesControl identifier choices
  | any (isJust . _dataTypeChoiceData) choices = []
  | otherwise =
    pure $
    Match () (ident "mkForm") [ PLit () (Signless ()) (String () identifierTxt identifierTxt)
                              , identPat "obj" ]
             (UnGuardedRhs () (apApp (varNamed "Left") [ body ])) Nothing
  where
    identifierTxt = identifier ^. _Identifier. unpacked
    choiceNamePrefix = "Data'" ++ identifierTxt ++ "_"
    choiceNm c = choiceNamePrefix ++ c ^. dataTypeChoiceConName . _Identifier . unpacked

    body = apApp (app (varNamed "ApiSettingsControlChoices")
                      [ liftHs False
                      , List () choicesHs ])
                 [ selection ]
    
    choicesHs = map mkHsChoice choices
    mkHsChoice choice = tuple [ text (choice ^. dataTypeChoiceConName . _Identifier)
                              , app (varNamed "encodeHapiValue") [ varNamed $ choiceNm choice ] ]

    selection =
      Case () (app (varNamed "runHapiParser")
                   [ app (varNamed "decodeHapiObject") [ varNamed "obj" ] ])
              ([ Alt () (PApp () (unqual "Left") [ PWildCard () ])
                        (UnGuardedRhs () (app (varNamed "pure") [ liftHs (0 :: Int) ])) Nothing ] ++
               zipWith (\i choice ->
                          Alt () (PApp () (unqual "Right") [ PApp () (unqual (choiceNm choice)) [] ])
                                 (UnGuardedRhs () (app (varNamed "pure") [ liftHs (i :: Int) ])) Nothing)
                       [0..] choices)

mkForm :: PackageName -> Identifier -> PluginCompoundType -> Match ()
mkForm thisPkg identifier ty =
  Match () (ident "mkForm") [ PLit () (Signless ()) (String () identifierTxt identifierTxt)
                            , PApp () (unqual "HapiObject") [ identPat "obj" ] ]
           (UnGuardedRhs () (apApp (varNamed "Right") [ body ])) (Just binds)
  where
    binds = BDecls () [ ctxtDecl ]
    ctxtDecl = PatBind () (identPat "varCtxt") (UnGuardedRhs () (app (varNamed "varCtxtFromObj") [ varNamed "obj"])) Nothing

    identifierTxt = identifier ^. _Identifier. unpacked
    body = apApp (varNamed "ApiSettingsDescription") [ sections, regularFields, app (varNamed "pure") [ varNamed "mempty" ] ]

    regularFields = apApp (varNamed "ApiSettingsSection") [ apApp (liftHs (Nothing :: Maybe ())) []
                                                          , sequenceAHs (foldMap mkField (ty ^. compoundTypeFields)) ]
    sections = sequenceAHs (map (\(section, fields) ->
                                   apApp (TupleSection () Boxed [Just (text (section ^. fieldSectionName . _Identifier)), Nothing])
                                         [ apApp (varNamed "ApiSettingsSection")
                                                 [ mkSectionDescription section
                                                 , sequenceAHs (foldMap mkField fields) ] ]) (ty ^. compoundTypeSections))

    sequenceAHs exps =
      app (varNamed "sequence") [ List () exps ]
    runIdlExpressionHs e =
      app (varNamed "runIdlExpression") [ varNamed "varCtxt", liftHs e ]
    lookupField field =
      app (varNamed "pure") [ app (varNamedQ "HM" "lookup") [ text (field ^. compoundFieldName . _Identifier), varNamed "obj" ] ]

    mkSectionDescription section =
      case findAttribute "description" (section ^. fieldSectionAttributes) of
        Just (AttributeValueExpression e) ->
          apApp (varNamed "Just") [ apApp (varNamed "mkHapiValueLabel") [runIdlExpressionHs e] ]
        _ -> apApp (varNamed "Nothing") []

    mkField field
      | Just AttributeValueOff <- findAttribute "visible" (field ^. compoundFieldAttrs) =
          mempty
      | otherwise =
        pure $
        apApp (varNamed "ApiSettingsField")
              [ app (varNamed "pure") [ text (field ^. compoundFieldName . _Identifier) ]
              , case findAttribute "name" (field ^. compoundFieldAttrs) of
                  Just (AttributeValueExpression e) ->
                    apApp (varNamed "mkHapiValueLabel") [runIdlExpressionHs e]
                  _ -> app (varNamed "pure") [ text ( field ^. compoundFieldName . _Identifier) ]
              , case findAttribute "description" (field ^. compoundFieldAttrs) of
                  Just (AttributeValueExpression e) ->
                    apApp (varNamed "Just") [ apApp (varNamed "mkHapiValueLabel") [runIdlExpressionHs e] ]
                  _ -> apApp (varNamed "Nothing") []
              , mkControl field (lookupField field) ]

    mkControl field v
      | Just AttributeValueOn <- findAttribute "readonly" (field ^. compoundFieldAttrs) =
          apApp (varNamed "ApiSettingsControlLabel")
                [ Do () [ Generator () (PApp () (unqual "Just") [ identPat "v" ]) v
                        , Qualifier () (app (varNamed "pure") [ app (varNamed "mkHapiValueLabel") [ varNamed "v" ] ]) ] ]
      | otherwise = mkControlForType (field ^. compoundFieldType) v

    mkControlForType (PluginTypeBuiltin BuiltinTypeString) v =
      apApp (varNamed "ApiSettingsControlText")
            [ Do () [ Generator () (PApp () (unqual "Just") [ PApp () (unqual "HapiValueText") [ identPat "v" ] ]) v
                    , Qualifier () (app (varNamed "pure") [ varNamed "v" ]) ] ]
    mkControlForType (PluginTypeBuiltin BuiltinTypeNumber) v =
      apApp (app (varNamed "ApiSettingsControlNumber") [ varNamed "False"
                                                       , tuple [ varNamed "Nothing", varNamed "Nothing" ] ])
            [ Do () [ Generator () (PApp () (unqual "Just") [ PApp () (unqual "HapiValueNumber") [ identPat "v" ] ]) v
                    , Qualifier () (app (varNamed "pure") [ varNamed "v" ]) ] ]
    mkControlForType (PluginTypeBuiltin BuiltinTypeTimestamp) v =
      apApp (varNamed "ApiSettingsControlCalendar")
            [ Do () [ Generator () (PApp () (unqual "Just") [ PApp () (unqual "HapiValueTimestamp") [ identPat "v" ] ]) v
                    , Qualifier () (app (varNamed "pure") [ varNamed "v" ]) ] ]
    mkControlForType (PluginTypeBuiltin BuiltinTypeFileUpload) v =
      apApp (varNamed "ApiSettingsControlFileUpload")
            [ Do () [ Generator () (PApp () (unqual "Just") [ PApp () (unqual "HapiValueFile") [ identPat "v" ] ]) v
                    , Qualifier () (If () (infixOp (varNamed "v") "==" (string ""))
                                          (app (varNamed "pure") [ varNamed "Nothing" ])
                                          (app (varNamed "fmap") [ app (varNamed "fmap") [ TupleSection () Boxed [ Nothing, Just $ varNamed "v" ] ]
                                                                 , app (varNamed "getFileNameFromURI") [ varNamed "v" ] ])) ] ]
    mkControlForType (PluginTypeBuiltin BuiltinTypeBool) v =
      apApp (varNamed "ApiSettingsControlCheckBox")
            [ Do () [ Generator () (PApp () (unqual "Just") [ PApp () (unqual "HapiValueSwitch") [ identPat "v" ] ]) v
                    , Qualifier () (app (varNamed "pure") [ varNamed "v" ]) ] ]
    mkControlForType (PluginTypeReference ref@(TypeName pkgName tyName)) v =
       let ref' = TypeName (maybe (Just thisPkg) Just pkgName) tyName
       in Do () [ Generator () (PApp () (unqual "Just") [ PApp () (unqual "HapiValueObject") [ identPat "objTy", identPat "obj" ] ]) v
                , Qualifier () (app (varNamed "guard") [ infixOp (varNamed "objTy") "==" (liftHs ref') ])
                , Qualifier () (app (varNamed "getControlForType") [liftHs ref, varNamed "obj"]) ]

    mkControlForType (PluginTypeOptional ty) v =
      let onJust = apApp (app (varNamed "ApiSettingsControlOptional") [ liftHs True ])
                         [mkControlForType ty pureV]
          onNothing =
            Do () [ Generator () (identPat "v") (app (varNamed "mkHapiConfigDefault") [ liftHs ty ])
                  , Qualifier () (apApp (app (varNamed "ApiSettingsControlOptional") [ liftHs False ])
                                        [mkControlForType ty pureV]) ]
          pureV = app (varNamed "pure") [app (varNamed "Just") [varNamed "v"]]
      in Do () [ Generator () (PApp () (unqual "Just") [ identPat "v" ]) v
               , Qualifier () $ Case () (varNamed "v")
                 [ Alt () (PApp () (unqual "HapiValueNothing") []) (UnGuardedRhs () onNothing) Nothing
                 , Alt () (PApp () (unqual "HapiValueJust") [identPat "x"]) (UnGuardedRhs () onJust) Nothing
                 , Alt () (PWildCard ()) (UnGuardedRhs () (app (varNamed "fail") [ string "Expected nothing or just for optional type" ])) Nothing ] ]
    mkControlForType (PluginTypeMany ty) v =
      mkControlForType ty v
    mkControlForType ty _ = error ("mkControl: " ++ show ty)

absTyName :: PackageName -> TypeName -> TypeName
absTyName pkg (TypeName Nothing ty) = TypeName (Just pkg) ty
absTyName _ ty = ty

compileConfigType :: PackageName -> PluginCompoundType -> Exp ()
compileConfigType pkgNm ty =
  let allFields = labeledCompoundFields ty
      mkFields = snd (foldl mkField (Nothing, id) allFields) []

      mkField :: (Maybe (Exp ()), [Stmt ()] -> [Stmt ()]) -> (String, PluginCompoundTypeField) -> (Maybe (Exp ()), [Stmt ()] -> [Stmt ()])
      mkField (vars, a) (vn, field) =
        case findAttribute "fill" (field ^. compoundFieldAttrs) of
          Just (AttributeValueExpression e) ->
            ( addVarToCtxt vars vn field
            , a . (Generator () (identPat vn) (app (varNamed "runIdlExpression") [ mkVarCtxt vars, liftHs e ]):) )
          _ ->
            case field ^. compoundFieldType of
              PluginTypeReference (TypeName Nothing localNm) ->
                ( addVarToCtxt vars vn field
                , a . (Generator () (identPat vn) (apApp (app (varNamed "HapiValueObject") [ liftHs (TypeName (Just pkgNm) localNm) ])
                                                         [ app (varNamed "fill") [ liftHs localNm, app (varNamed "HapiObject") [ varNamed "mempty" ] ] ]):))
              fieldType ->
                ( addVarToCtxt vars vn field
                , a . (Generator () (identPat vn) (app (varNamed "mkHapiConfigDefault") [ liftHs (field ^. compoundFieldType) ]):) )

      doReturn = Qualifier () (app (varNamed "pure") [ app (varNamed "HapiObject") [ app (varNamedQ "HM" "fromList") [ List () returnFields ]]])
      returnFields = map (\(vn, field) -> tuple [ text (field ^. compoundFieldName . _Identifier), varNamed vn ]) allFields
  in Do () (mkFields ++
            [doReturn])

compileIdl :: PluginDescr -> ApiDescription
compileIdl descr =
  ApiDescription
  { _apiDescriptionName = descr ^. pluginDescrPackage . _PackageName . re _ApiName
  , _apiDescriptionLookups = do
      (_, PluginDeclLookup lookup) <- descr ^. pluginDescrDecls
      pure ApiLookupDescription
        { _apiLookupName = lookup ^. lookupName
        , _apiLookupScope =
            case findAttribute "scope" (lookup ^. lookupAttributes) of
              Just (AttributeValueExpression (ExpressionIdentifier (QualifiedIdentifier Nothing "global"))) ->
                ApiScopeGlobal
              Just (AttributeValueExpression (ExpressionIdentifier (QualifiedIdentifier Nothing "user"))) ->
                ApiScopeUser
              Just (AttributeValueExpression (ExpressionIdentifier (QualifiedIdentifier Nothing "api_instance"))) ->
                ApiScopeInstance
              Just e -> error ("Unknown scope declaration " ++ show e)
              Nothing -> ApiScopeGlobal
        , _apiLookupVisible = ApiVisibilityPublic
        , _apiLookupKey = compileType pkg (lookup ^. lookupKey)
        , _apiLookupValue = compileType pkg (lookup ^. lookupValues)
        , _apiLookupDescriptionUrl = Nothing
        , _apiLookupUrl = Nothing }
  , _apiDescriptionTypes = []
  , _apiDescriptionRequests = do
      (_, PluginDeclRequest request) <- descr ^. pluginDescrDecls
      pure ApiRequestDescription
        { _apiRequestName = request ^. requestName . _Identifier . re _ApiRequestName
        , _apiRequestUrl = Nothing
        , _apiRequestScope   =
            case findAttribute "scope" (request ^. requestAttributes) of
              Just (AttributeValueExpression (ExpressionIdentifier (QualifiedIdentifier Nothing "global"))) ->
                ApiScopeGlobal
              Just (AttributeValueExpression (ExpressionIdentifier (QualifiedIdentifier Nothing "user"))) ->
                ApiScopeUser
              Just (AttributeValueExpression (ExpressionIdentifier (QualifiedIdentifier Nothing "api_instance"))) ->
                ApiScopeInstance
              Just e -> error ("Unknown scope declaration " ++ show e)
              Nothing -> ApiScopeGlobal
        , _apiRequestVisible = ApiVisibilityPublic
        , _apiRequestInput = compileType pkg (request ^. requestArgs)
        , _apiRequestOutput = typeRef pkg (request ^. requestResultType)
        }
  , _apiDescriptionCreateUrl = Nothing
  }
  where
    pkg = descr ^. pluginDescrPackage

compileType :: PackageName -> PluginCompoundType -> ApiCompoundTypeSchema
compileType pkg ty =
  ApiCompoundTypeSchema (map (compileField pkg) (ty ^. compoundTypeFields))
                        (map (\(nm, fields) -> ( nm ^. fieldSectionName
                                               , sectionDescription (nm ^. fieldSectionAttributes)
                                               , map (compileField pkg) fields)) (ty ^. compoundTypeSections))
  where
    sectionDescription atts =
      case find ((== "description") . view attrKey) atts of
        Just (Attribute _ (AttributeValueExpression (ExpressionValue (ValueString s)))) -> s
        _ -> ""

compileField pkg f =
  ApiCompoundFieldSchema (f ^. compoundFieldName) (typeRef pkg (f ^. compoundFieldType))
                         ApiVisibilityPublic False ""

typeRef :: PackageName -> PluginType -> ApiTypeDescription
typeRef pkg (PluginTypeMany m) = ApiTypeMany (typeRef pkg m)
typeRef pkg (PluginTypeOptional m) = ApiTypeOptional (typeRef pkg m)
typeRef pkg (PluginTypeTuple ms) = ApiTypeTuple (map (typeRef pkg) ms)
typeRef _ (PluginTypeBuiltin b) =
  ApiTypeDescription
  { _apiTypeName = case b of
                     BuiltinTypeString -> "string"
                     BuiltinTypeBool   -> "boolean"
                     BuiltinTypeTimestamp -> "timestamp"
                     BuiltinTypeDecimal -> "decimal"
                     BuiltinTypeNumber -> "number"
  , _apiTypeSource = ApiTypeSourceBuiltin
  , _apiTypeDescriptionUrl = Nothing }
typeRef pkg (PluginTypeReference (TypeName tyPkg nm)) =
  ApiTypeDescription
  { _apiTypeName = nm
  , _apiTypeSource = ApiTypeSourcePackage (fromMaybe pkg tyPkg) Nothing
  , _apiTypeDescriptionUrl = Nothing }

-- * liftHs

class LiftHs a where
  liftHs :: a -> Exp ()

  default liftHs :: (Generic a, GLiftHs (Rep a)) => a -> Exp ()
  liftHs = gLiftHs . from

  liftHsList :: [a] -> Exp ()
  liftHsList = liftHs
instance LiftHs Char where
  liftHs c = Lit () (Char () c [c])
  liftHsList = string
instance LiftHs T.Text where
  liftHs = text
instance LiftHs Integer where
  liftHs i = Lit () (Int () i (show i))
instance LiftHs Int where
  liftHs i = Lit () (Int () (fromIntegral i) (show i))
instance LiftHs a => LiftHs (Maybe a) where
  liftHs Nothing = varNamed "Nothing"
  liftHs (Just a) = app (varNamed "Just") [ liftHs a ]

instance LiftHs a => LiftHs [a] where
  liftHs = List () . map liftHs

instance LiftHs () where
  liftHs () = tuple []
instance (LiftHs a, LiftHs b) => LiftHs (a, b) where
  liftHs (a, b) = tuple [liftHs a, liftHs b]
instance (LiftHs a, LiftHs b, LiftHs c) => LiftHs (a, b, c) where
  liftHs (a, b, c) = tuple [liftHs a, liftHs b, liftHs c]

class GLiftHs rep where
  gLiftHs :: rep () -> Exp ()
instance GLiftHs f => GLiftHs (D1 c f) where
  gLiftHs (M1 x) = gLiftHs x
instance (GLiftHs a, GLiftHs b) => GLiftHs (a :+: b) where
  gLiftHs (L1 x) = gLiftHs x
  gLiftHs (R1 x) = gLiftHs x
instance (KnownSymbol nm, GLiftRecord f) => GLiftHs (C1 ('MetaCons nm x 'True) f) where
  gLiftHs (M1 x) = RecConstr () (unqual (symbolVal (Proxy @nm))) (liftRecord x)
instance (KnownSymbol nm, GLiftFields f) => GLiftHs (C1 ('MetaCons nm x 'False) f) where
  gLiftHs (M1 x) = app  (varNamed (symbolVal (Proxy @nm))) (liftFields x)

class GLiftRecord rep where
  liftRecord :: rep () -> [ FieldUpdate () ]
instance (GLiftRecord a, GLiftRecord b) => GLiftRecord (a :*: b) where
  liftRecord (a :*: b) = liftRecord a ++ liftRecord b
instance (KnownSymbol nm, LiftHs a) => GLiftRecord (S1 ('MetaSel ('Just nm) x y z) (Rec0 a)) where
  liftRecord (M1 (K1 x)) = [ field (symbolVal (Proxy @nm)) (liftHs x) ]

class GLiftFields rep where
  liftFields :: rep () -> [ Exp () ]
instance (GLiftFields a, GLiftFields b) => GLiftFields (a :*: b) where
  liftFields (a :*: b) = liftFields a ++ liftFields b
instance LiftHs a => GLiftFields (S1 ('MetaSel 'Nothing x y z) (Rec0 a)) where
  liftFields (M1 (K1 x)) = [ liftHs x ]
instance GLiftFields U1 where
  liftFields _ = []

instance LiftHs UTCTime where
  liftHs _ = error "LiftHs{UTCTime}"
instance LiftHs Scientific where
  liftHs s = Lit () (Frac () (toRational s) (show s))

instance LiftHs ApiDescription
instance LiftHs ApiName
instance LiftHs ApiLookupDescription
instance LiftHs ApiTypeDescription
instance LiftHs ApiRequestDescription
instance LiftHs ApiTypeSource
instance LiftHs ApiCompoundFieldSchema
instance LiftHs ApiCompoundTypeSchema
instance LiftHs Identifier
instance LiftHs ApiRequestName
instance LiftHs ApiEntityScope
instance LiftHs ApiEntityVisibility
instance LiftHs URI where
  liftHs uri = app (varNamed "read") [ string (show uri) ]
instance LiftHs Bool
instance LiftHs PackageName
instance LiftHs Expression
instance LiftHs Value
instance LiftHs PluginType
instance LiftHs BuiltinType
instance LiftHs QualifiedIdentifier
instance LiftHs TypeName
instance LiftHs PluginCompoundType
instance LiftHs PluginCompoundTypeField
instance LiftHs PluginFieldSection
instance LiftHs Attribute
instance LiftHs AttributeValue
instance LiftHs StringPiece
