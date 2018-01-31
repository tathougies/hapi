module Web.Hapi.Plugin.Idl where

import           Control.Lens hiding ((.=))
import           Control.Monad

import           Data.Aeson hiding (Value)
import           Data.Char
import           Data.Data
import           Data.Hashable
import           Data.List (find)
import           Data.Monoid
import           Data.Scientific
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time

import           GHC.Generics (Generic)

import           Servant

import           Text.Megaparsec
--import           Text.Megaparsec.Expr
import qualified Text.Megaparsec.Lexer as L

newtype Identifier = Identifier { fromIdentifier :: Text }
  deriving (Show, Eq, IsString, ToJSON, FromJSON, Generic, Data, ToHttpApiData, FromHttpApiData, Hashable)
makePrisms ''Identifier
newtype PackageName = PackageName { fromPackageName :: Text }
  deriving (Show, Eq, IsString, ToJSON, FromJSON, Generic, Data)
makePrisms ''PackageName
newtype Markup = Markup { fromMarkup :: Text }
  deriving (Show, Eq, IsString, Generic)
makeLenses ''Markup

data TypeName = TypeName (Maybe PackageName) Identifier
  deriving (Show, Eq, Data, Generic)

data BuiltinType
  = BuiltinTypeString
  | BuiltinTypeTimestamp
  | BuiltinTypeBool
  | BuiltinTypeDecimal
  | BuiltinTypeNumber
  | BuiltinTypeFileUpload
  deriving (Show, Generic)
instance ToJSON BuiltinType
instance FromJSON BuiltinType

data PluginType
  = PluginTypeMany !PluginType
  | PluginTypeOptional !PluginType
  | PluginTypeTuple [PluginType]
  | PluginTypeReference !TypeName
  | PluginTypeBuiltin !BuiltinType
  deriving (Show, Generic)
instance ToJSON PluginType
instance FromJSON PluginType

data PluginDataTypeDecl
  = PluginDataTypeChoices [ PluginDataTypeChoice ]
  | PluginDataTypeOpaque  PluginOpaqueData
  | PluginDataTypeData    PluginCompoundType
  deriving Show

data PluginDataTypeChoice
  = PluginDataTypeChoice
  { _dataTypeChoiceConName :: Identifier
  , _dataTypeChoiceData    :: Maybe TypeName
  } deriving Show

data PluginOpaqueData
  = PluginOpaqueData
  { _opaqueDataScheme :: Identifier
  } deriving Show

data PluginFieldSection
  = PluginFieldSection
  { _fieldSectionName :: Identifier
  , _fieldSectionAttributes :: [ Attribute ]
  } deriving (Show, Generic)

data PluginCompoundType
  = PluginCompoundType
  { _compoundTypeFields :: [ PluginCompoundTypeField ]
  , _compoundTypeSections :: [ (PluginFieldSection, [PluginCompoundTypeField]) ]
  } deriving (Show, Generic)

compoundTypeAllFields :: Traversal' PluginCompoundType PluginCompoundTypeField
compoundTypeAllFields f c =
  PluginCompoundType <$> traverse f (_compoundTypeFields c)
                     <*> traverse (\(s, fields) -> (s,) <$> traverse f fields) (_compoundTypeSections c)

data PluginCompoundTypeField
  = PluginCompoundTypeField
  { _compoundFieldName :: Identifier
  , _compoundFieldType :: PluginType

  , _compoundFieldAttrs :: [ Attribute ]
  } deriving (Show, Generic)

data Attribute
  = Attribute
  { _attrKey :: Identifier
  , _attrValue :: AttributeValue
  } deriving (Show, Generic)

data AttributeValue
  = AttributeValueExpression Expression
  | AttributeValueOn | AttributeValueOff
  deriving (Show, Generic)

data QualifiedIdentifier
  = QualifiedIdentifier
  { _qualPackage :: Maybe PackageName
  , _qualIdentifier :: Identifier
  } deriving (Show, Generic)

data Value
  = ValueNumber !Integer
  | ValueTimestamp !UTCTime
  | ValueBool   !Bool
  | ValueString !Text
  | ValueDecimal !Scientific
  deriving (Show, Generic)

data Expression
  = ExpressionIdentifier QualifiedIdentifier
  | ExpressionGetProperty Expression Identifier
  | ExpressionBinOp Text Expression Expression
  | ExpressionUnOp Text Expression
  | ExpressionApply QualifiedIdentifier [(Identifier, Expression)]
  | ExpressionValue Value
  | ExpressionString [StringPiece]
  | ExpressionMarkdown [StringPiece]
  | ExpressionTuple [ Expression ]
  | ExpressionMany [ Expression ]
  deriving (Show, Generic)

data StringPiece
  = StringPieceText !Text
  | StringPieceExpression !Expression
  deriving (Show, Generic)

data PluginRequestDecl
  = PluginRequestDecl
  { _requestName :: Identifier
  , _requestArgs :: PluginCompoundType
  , _requestResultType :: PluginType
  , _requestAttributes :: [Attribute]
  } deriving Show

data PluginLookupDecl
  = PluginLookupDecl
  { _lookupName :: Identifier
  , _lookupValues :: PluginCompoundType
  , _lookupKey    :: PluginCompoundType
  , _lookupAttributes :: [Attribute]
  } deriving Show

data PluginDecl
  = PluginDeclDataType PluginDataTypeDecl [Attribute]
  | PluginDeclRequest  PluginRequestDecl
  | PluginDeclLookup   PluginLookupDecl
  deriving Show

data PluginDescr
  = PluginDescr
  { _pluginDescrPackage :: PackageName
  , _pluginDescrAttrs   :: [ Attribute ]
  , _pluginDescrDecls   :: [ (Identifier, PluginDecl) ]
  } deriving Show

makePrisms ''TypeName
makePrisms ''BuiltinType
makePrisms ''PluginType
makePrisms ''PluginDataTypeDecl
makeLenses ''PluginDataTypeChoice
makeLenses ''PluginOpaqueData
makeLenses ''PluginFieldSection
makeLenses ''PluginCompoundType
makeLenses ''PluginCompoundTypeField
makeLenses ''Attribute
makePrisms ''AttributeValue
makeLenses ''QualifiedIdentifier
makePrisms ''Value
makePrisms ''Expression
makeLenses ''PluginRequestDecl
makeLenses ''PluginLookupDecl
makePrisms ''PluginDecl
makeLenses ''PluginDescr
makePrisms ''StringPiece

-- * Parser

type Parser = Parsec Dec String

sc :: Parser ()
sc = L.space (skipSome spaceChar) lineCmt blockCmt
  where
    lineCmt = L.skipLineComment "--"
    blockCmt = L.skipBlockComment "{-" "-}"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser ()
symbol s = void (L.symbol sc s)

brackets, parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
brackets = between (symbol "[") (symbol "]")

commas :: Parser a -> Parser [a]
commas = flip sepBy (symbol ",")

semi :: Parser ()
semi = symbol ";"

rword :: String -> Parser ()
rword w = lexeme (string w *> notFollowedBy alphaNumChar)

rws :: [String]
rws = [ "service", "opaque", "data", "lookup", "finds", "by", "choices", "request", "optional", "many"
      , "string", "timestamp", "boolean", "number", "decimal", "true", "false", "fileupload" ]

identifier :: Parser Identifier
identifier = fmap (Identifier . fromString)
             ((lexeme . try) (p >>= check))
  where
    p = (:) <$> (char '_' <|> letterChar) <*> many (char '-' <|> char '_' <|> alphaNumChar)
    check x
      | x `elem` rws = fail $ "Keyword " ++ x ++ " cannot be used as an identifier"
      | otherwise = pure x

stringLiteral :: Parser Text
stringLiteral = lexeme (fmap fromString (char '"' *> stringLiteral' <* char '"'))

stringLiteral' :: Parser [Char]
stringLiteral' = ((:) <$> (escapeChar <|> satisfy (/= '"')) <*> stringLiteral') <|> pure []

escapeChar :: Parser Char
escapeChar = char '\\' *>
             ('\t' <$ char 't' <|>
              '\a' <$ char 'a' <|>
              '\v' <$ char 'v' <|>
              '\n' <$ char 'n' <|>
              '\r' <$ char 'r' <|>
              '$'  <$ char '$' <|>
              (char 'x' *> hexP))
  where
    hexP = do
      a <- hexVal <$> hexDigitChar
      b <- hexVal <$> hexDigitChar
      (try $ do { c <- hexVal <$> hexDigitChar
                ; d <- hexVal <$> hexDigitChar
                ; pure (chr (a * 0x1000 + b * 0x100 + c * 0x10 + d)) } <|>
       pure (chr (a * 0x10 + b)))

    hexVal = digitToInt

stringExpression :: Parser [StringPiece]
stringExpression = lexeme (char '"' *> stringPieces (char '"') id [])

stringPieces :: Parser a -> ([StringPiece] -> [StringPiece]) -> [Char] -> Parser [StringPiece]
stringPieces endOf pieces s =
  (endOf *>
   case s of
     [] -> pure (pieces [])
     _ -> pure (pieces . (StringPieceText (T.reverse (fromString s)):) $ [])) <|>
  do { let pieces' =
             case s of
               [] -> pieces
               _  -> pieces . (StringPieceText (T.reverse (fromString s)):)
     ; pc <- char '$' *> char '{' *> (StringPieceExpression <$> expression) <* char '}'
     ; stringPieces endOf (pieces' . (pc:)) [] } <|>
  do { c <- escapeChar <|> satisfy (\c -> c /= '"' && c /= '$')
     ; stringPieces endOf pieces (c:s) }

quotedIdentifier :: Parser Identifier
quotedIdentifier = Identifier <$> stringLiteral

qualified' :: Parser (Maybe Text, Text)
qualified' = go Nothing
  where
    go pkgName = do
      Identifier nextChunk <- identifier
      (char '.' *> go (Just (maybe nextChunk (\p -> p <> "." <> nextChunk) pkgName))) <|> pure (pkgName, nextChunk)

qualified :: Parser QualifiedIdentifier
qualified = do
  (pkg, ident) <- qualified'
  pure (QualifiedIdentifier (fmap PackageName pkg) (Identifier ident))

packageName :: Parser PackageName
packageName = do
  (pkg, ident) <- qualified'
  case pkg of
    Nothing -> pure (PackageName ident)
    Just pkgName -> pure (PackageName (pkgName <> "." <> ident))

pluginDescr :: Parser PluginDescr
pluginDescr =
  uncurry PluginDescr
    <$> serviceDecl
    <*> many (pluginDecl <* semi)
  where
    serviceDecl = rword "service" *> ((,) <$> (packageName <?> "Package name")
                                          <*> attributes) <* semi

pluginDecl :: Parser (Identifier, PluginDecl)
pluginDecl = do { rword "opaque"; rword "data"
                ; (nm, decl) <- opaqueDecl
                ; attrs <- attributes
                ; pure (nm, PluginDeclDataType (PluginDataTypeOpaque decl) attrs) } <|>
             do { rword "choices"
                ; (nm, decl) <- choicesDecl
                ; attrs <- attributes
                ; pure (nm, PluginDeclDataType (PluginDataTypeChoices decl) attrs) } <|>
             do { rword "data"
                ; (nm, decl) <- compoundTypeDecl
                ; attrs <- attributes
                ; pure (nm, PluginDeclDataType (PluginDataTypeData decl) attrs) } <|>
             fmap (fmap PluginDeclRequest)                            (rword "request" *> requestDecl) <|>
             fmap (fmap PluginDeclLookup)                             (rword "lookup" *> lookupDecl) <?>
             "top-level declaration"

opaqueDecl :: Parser (Identifier, PluginOpaqueData)
opaqueDecl = (,) <$> identifier <*> fmap PluginOpaqueData quotedIdentifier

choicesDecl :: Parser (Identifier, [PluginDataTypeChoice])
choicesDecl = (,) <$> identifier <*> parens (commas choiceP)
  where
    choiceP = PluginDataTypeChoice <$> identifier
                                   <*> try (optional typeName)

typeName :: Parser TypeName
typeName = do
  (pkg, nm) <- qualified'
  pure (TypeName (fmap PackageName pkg) (Identifier nm))

expression :: Parser Expression
expression = term' <?> "expression"
  where
    term = parens (do { e <- expression
                      ; try (symbol "," *> fmap ExpressionTuple ((e:) <$> commas expression)) <|>
                        pure e }) <|>
           ExpressionValue <$> literal <|>
           ExpressionString <$> stringExpression <|>
           ExpressionString <$> (char '>' *> hereDoc) <|>
           ExpressionMarkdown <$> (char '$' *> char '>' *> hereDoc) <|>
           ExpressionMany <$> brackets (commas expression) <|>
           functionCall <?> "term"

    term' = do
      e <- term
      try (char '.' *> (ExpressionGetProperty e <$> identifier)) <|>
        pure e

    literal = do { n <- lexeme L.number
                 ; either (\(_ :: Double) -> pure (ValueDecimal n)) (pure . ValueNumber) (floatingOrInteger n)
                 } <|>
              ValueBool True <$ rword "true" <|>
              ValueBool False <$ rword "false" <?>
              "literal"

    functionCall = do
      nm <- char '.' *> qualified <|>
            QualifiedIdentifier Nothing <$> identifier
      fmap (ExpressionApply nm) (parens (fields id)) <|>
        pure (ExpressionIdentifier nm)

    fields a = do
      nm <- try (Just <$> identifier) <|> pure Nothing
      case nm of
        Nothing -> pure (a [])
        Just nm' -> do
          symbol "="
          e <- lexeme expression

          let a' = a . ((nm', e):)
          try (do { symbol ";"
                  ; try (fields a') <|> pure (a' []) }) <|>
             pure (a' [])


hereDoc :: Parser [StringPiece]
hereDoc = do
  _ <- many (satisfy (\c -> c /= '\n' && isSpace c))
  _ <- char '\n'

  level <- docLineBeginning
  firstLine <- docLineRest

  lines' <- many (docLine level)
  sc

  pure (mconcat (map (<> [StringPieceText "\n"]) (firstLine:lines')))

  where
    docLine pos = do
      try (do pos' <- docLineBeginning
              when (pos /= pos') (fail "Here doc indentation mismatch"))
      docLineRest

    docLineBeginning = do { _ <- many (satisfy (\c -> c /= '\n' && c /= '>' && isSpace c))
                          ; L.indentLevel <* (char '>' <?> "here doc line beginning") } <?> "here doc line"
    docLineRest = do
      _ <- many (satisfy (\c -> c /= '>' && c /= '\n' && isSpace c))
      stringPieces (char '\n') id mempty

compoundTypeDecl :: Parser (Identifier, PluginCompoundType)
compoundTypeDecl = (,) <$> identifier <*> compoundType

compoundType :: Parser PluginCompoundType
compoundType = parens (PluginCompoundType <$> many pluginField
                                          <*> many ((,) <$> pluginFieldSection <*> many pluginField))

pluginFieldSection :: Parser PluginFieldSection
pluginFieldSection = brackets (PluginFieldSection <$> (quotedIdentifier <?> "section name") <*> attributes')

pluginField :: Parser PluginCompoundTypeField
pluginField = do
  ty <- try pluginType
  nm <- identifier
  attrs <- attributes
  semi
  pure (PluginCompoundTypeField nm ty attrs)

attributes' :: Parser [Attribute]
attributes' = many (exprAttribute <|> onAttribute <|> offAttribute <?> "attribute")
  where
    exprAttribute =
      Attribute <$> try (identifier <* symbol "=")
                <*> fmap AttributeValueExpression expression
    onAttribute  =
      Attribute <$> identifier <*> pure AttributeValueOn
    offAttribute =
      Attribute <$> (char '!' *> identifier) <*> pure AttributeValueOff

attributes :: Parser [Attribute]
attributes = brackets attributes' <|> pure []

pluginType :: Parser PluginType
pluginType = parens typeOrTuple <|>
             try (PluginTypeBuiltin BuiltinTypeString    <$ rword "string") <|>
             try (PluginTypeBuiltin BuiltinTypeTimestamp <$ rword "timestamp") <|>
             try (PluginTypeBuiltin BuiltinTypeBool      <$ rword "boolean") <|>
             try (PluginTypeBuiltin BuiltinTypeDecimal   <$ rword "decimal") <|>
             try (PluginTypeBuiltin BuiltinTypeFileUpload <$ rword "fileupload") <|>
             try (PluginTypeBuiltin BuiltinTypeNumber    <$ rword "number") <|>
             try (rword "many" *> fmap PluginTypeMany pluginType) <|>
             try (rword "optional" *> fmap PluginTypeOptional pluginType) <|>
             PluginTypeReference <$> typeName <?>
             "type"
  where
    typeOrTuple = do
      t <- pluginType
      try (fmap PluginTypeTuple (symbol "," *> fmap (t:) (many pluginType)) <|>
           pure t)

requestDecl :: Parser (Identifier, PluginRequestDecl)
requestDecl = do
  nm <- identifier
  fmap (nm,)
       (PluginRequestDecl nm <$> compoundType
                             <*> (symbol "->" *> pluginType)
                             <*> attributes)

lookupDecl :: Parser (Identifier, PluginLookupDecl)
lookupDecl = do
  nm <- identifier
  fmap (nm,)
       (PluginLookupDecl nm <$> (rword "finds" *> compoundType)
                            <*> (rword "by"    *> compoundType)
                            <*> attributes)

parseFile :: FilePath -> IO PluginDescr
parseFile fp = do
  fileData <- readFile fp
  case parse pluginDescr fp fileData of
    Left p -> fail ("Error: " ++ show p)
    Right x -> pure x

findAttribute :: Identifier -> [Attribute] -> Maybe AttributeValue
findAttribute i =
  fmap (view attrValue) .
  find ((==i) . view attrKey)

-- * Aeson instances

instance ToJSON TypeName where
  toJSON (TypeName Nothing (Identifier x)) = toJSON x
  toJSON (TypeName (Just (PackageName pkgName)) (Identifier x)) =
    object [ "$package" .= pkgName, "$type" .= x ]
instance FromJSON TypeName where
  parseJSON x = TypeName Nothing . Identifier <$> parseJSON x <|>
                withObject "TypeName" (\o -> TypeName <$> (Just . PackageName <$> o .: "$package")
                                                      <*> (Identifier <$> o .: "$type")) x
