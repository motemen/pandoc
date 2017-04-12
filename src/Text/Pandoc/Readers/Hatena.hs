module Text.Pandoc.Readers.Hatena (readHatena) where

import Control.Monad.Except (throwError)
import Data.Default
import Data.Char (isHexDigit)
import Text.HTML.TagSoup
import Text.Pandoc.Builder (Blocks, Inlines, (<>))
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Class (PandocMonad)
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Parsing

readHatena :: PandocMonad m
           => ReaderOptions -- ^ Reader options
           -> String        -- ^ String to parse (assuming @'\n'@ line endings)
           -> m Pandoc
readHatena opts s = do
  parsed <- (readWithM parseHatena) def{ parserState = def{ stateOptions = opts } } s
  case parsed of
    Right result -> return result
    Left e       -> throwError e

data HatenaState = HatenaState { parserState :: ParserState
                               , stSuppressP :: Bool
                               }
instance Default HatenaState where
  def = HatenaState { parserState = def, stSuppressP = False }

instance HasReaderOptions HatenaState where
  extractReaderOptions = extractReaderOptions . parserState

instance HasHeaderMap HatenaState where
  extractHeaderMap = extractHeaderMap . parserState
  updateHeaderMap f s = s{ parserState = updateHeaderMap f $ parserState s }

instance HasLogMessages HatenaState where
  addLogMessage m s = s{ parserState = addLogMessage m $ parserState s }
  getLogMessages = getLogMessages . parserState

instance HasIdentifierList HatenaState where
  extractIdentifierList     = extractIdentifierList . parserState
  updateIdentifierList f st = st{ parserState = updateIdentifierList f $ parserState st }

type HatenaParser m = ParserT [Char] HatenaState m

parseHatena :: PandocMonad m => HatenaParser m Pandoc
parseHatena = do
  state <- getState
  blocks <- B.toList <$> parseBlocks
  let meta = stateMeta $ parserState state
  return $ Pandoc meta blocks

parseBlocks :: PandocMonad m => HatenaParser m Blocks
parseBlocks = mconcat <$> manyTill block eof

block :: PandocMonad m => HatenaParser m Blocks
block = choice [ list
               , blockQuote
               , codeBlock
               , table
               , definitionList
               , header
               , suppressPara
               , linebreaks
               , para
               ] <?> "block"

inline :: PandocMonad m => HatenaParser m Inlines
inline = try inline'
  where
    inline' = choice [ bracket
                     , B.str "((" <$ (try $ string ")((")
                     , B.str "))" <$ (try $ string "))(")
                     , htmlTag
                     , footnote
                     , autolink
                     , htmlEntity
                     , str
                     , symbol
                     ] <?> "inline"

footnote :: PandocMonad m => HatenaParser m Inlines
footnote = try $ do
  string "(("
  content <- many1Till (noneOf "\n") (try $ string "))")
  return $ B.note $ B.plain $ B.str content

str :: PandocMonad m => HatenaParser m Inlines
str = do
  B.str <$> (:[]) <$> (noneOf "[]<>()|:-+*\n")

htmlEntity :: PandocMonad m => HatenaParser m Inlines
htmlEntity = try $ do
    char '&'
    c <- choice [ name, dec, hex ]
    return $ B.rawInline "html" $ "&" ++ c ++ ";"
  where
    name = try $ do
      many1Till alphaNum (char ';')
    dec = try $ do
      (++) <$> string "#" <*> many1Till digit (char ';')
    hex = try $ do
      (++) <$> string "#x" <*> many1Till (satisfy isHexDigit) (char ';')

suppressPara :: PandocMonad m => HatenaParser m Blocks
suppressPara = try $ do
  string "><"
  html <- many1Till anyChar (try $ string "><\n")
  st <- getState
  setState st{ stSuppressP = True }
  contents <- parseFromString parseBlocks ("<"++html++">\n")
  setState st
  return contents

table :: PandocMonad m => HatenaParser m Blocks
table = try $ do
  hdr <- map (B.plain . B.str) <$> many1Till (string "|*" >> many (noneOf "|\n")) (try $ string "|\n")
  rows <- many $ map (B.plain . B.str) <$> many1Till (string "|" >> many (noneOf "|\n")) (try $ string "|\n")
  return $ B.simpleTable hdr rows

definitionList :: PandocMonad m => HatenaParser m Blocks
definitionList = do
  B.definitionList <$> many1 definitionListItem
  where
    definitionListItem = try $ do
      char ':'
      term <- mconcat <$> many1Till inline (char ':')
      defn <- B.plain <$> mconcat <$> many1Till inline (char '\n')
      return $ (term, [defn])


symbol :: PandocMonad m => HatenaParser m Inlines
symbol = do
  B.str <$> (:[]) <$> oneOf "[]<>()|:-+*"

autolink :: PandocMonad m => HatenaParser m Inlines
autolink = try $ do
  u <- httpUri
  return $ B.link u "" (B.str u)

header :: PandocMonad m => HatenaParser m Blocks
header = try $ do
  char '*'
  name <- option "" $ manyTill alphaNum (lookAhead $ char '*')
  marker <- many $ char '*'
  skipSpaces
  attr <- if null name
             then return nullAttr
             else registerHeader nullAttr (B.str name)
  B.headerWith attr (length marker + 1) <$> line

linebreaks :: PandocMonad m => HatenaParser m Blocks
linebreaks = do
  nls <- many1 newline
  return $ B.plain $ mconcat $ take (length nls - 1) $ repeat B.linebreak

httpUri :: PandocMonad m => HatenaParser m String
httpUri = do
  lookAhead $ try $ choice [ string "http://", string "https://" ]
  (origUri, _) <- uri
  return origUri

-- TODO: math
bracket :: PandocMonad m => HatenaParser m Inlines
bracket = try $ do
  char '['
  origUri <- httpUri
  uriRest <- manyTill (noneOf "]\n") (char ']')
  (url, UrlModifier title isImage) <- parseFromString urlWithModifier (origUri ++ uriRest)
  if isImage
    then return $ B.image url "" mempty
    else return $ B.link url "" (B.str $ if null title then url else title)

data UrlModifier = UrlModifier { urlModTitle :: String, urlModIsImage :: Bool }

nullUrlModifier :: UrlModifier
nullUrlModifier = UrlModifier { urlModTitle = "", urlModIsImage = False }

urlWithModifier :: PandocMonad m => HatenaParser m (String, UrlModifier)
urlWithModifier = do
    url <- many1Till anyChar (lookAhead $ try $ urlModifier *> eof)
    mods <- urlModifier
    return (url, mods)

urlModifier :: PandocMonad m => HatenaParser m UrlModifier
urlModifier = do
  mods <- many $ choice [ modTitle
                        , modImage
                        ]
  return $ foldl (.) id mods nullUrlModifier
  where
    modTitle = try $ do
      string ":title="
      title <- many1 $ noneOf ":"
      return $ \o -> o { urlModTitle = title }
    modImage = try $ do
      string ":image"
      return $ \o -> o { urlModIsImage = True }

htmlTag :: PandocMonad m => HatenaParser m Inlines
htmlTag = choice [ htmlTagCode
                 , htmlTagAny
                 ]

htmlTagCode :: PandocMonad m => HatenaParser m Inlines
htmlTagCode = try $ do
  string "<code>"
  content <- manyTill (noneOf "\n") (string "</code>")
  return $ case parseTags content of
    [TagText text] -> B.code text
    _ -> B.rawInline "html" $ "<code>" ++ content ++ "</code>"

htmlTagAny :: PandocMonad m => HatenaParser m Inlines
htmlTagAny = try $ do
  char '<'
  tag <- many1Till (noneOf "\n") (char '>')
  return $ B.rawInline "html" ("<"++tag++">")

para :: PandocMonad m => HatenaParser m Blocks
para = do
  noP <- stSuppressP <$> getState
  (if noP then B.plain else B.para) <$> line

line :: PandocMonad m => HatenaParser m Inlines
line = mconcat <$> many1Till inline (char '\n')

blockQuote :: PandocMonad m => HatenaParser m Blocks
blockQuote = try $ do
  string ">"
  cite <- option Nothing $ Just . fst <$> uri
  string ">\n"
  contents <- mconcat <$> manyTill block (try $ string "<<\n")
  return $ B.blockQuote
         $ contents
           <> case cite of
                Nothing -> mempty
                Just u  -> B.plain $ B.rawInline "html" "<cite>"
                                   <> B.link u "" (B.str u)
                                   <> B.rawInline "html" "</cite>"

codeBlock :: PandocMonad m => HatenaParser m Blocks
codeBlock = try $ do
  string ">|"
  lang <- manyTill (noneOf "|\n") (string "|\n")
  contents <- mconcat <$> map (++ "\n") <$> manyTill anyLine (try $ string "||<\n")
  return $ B.codeBlockWith (langAttr lang) contents
  where
    langAttr ""    = nullAttr
    langAttr lang  = ("", [lang], [])

list :: PandocMonad m => HatenaParser m Blocks
list = buildListFromItems <$> many1 (listItem 0)

listItem :: PandocMonad m => Int -> HatenaParser m (ListMarker, Blocks)
listItem depth = try $ do
  count depth listMarker
  mark <- listMarker
  item <- subList <|> listItemContent
  return (mark, item)
  where
    listMarker = (BulletListMarker <$ char '-') <|> (OrderedListMarker <$ char '+')
    listItemContent = do
      skipSpaces
      B.plain <$> line
    subList = try $ do
      mark <- listMarker
      item <- listItemContent
      rest <- many (listItem $ depth+1)
      return $ buildListFromItems $ (mark, item) : rest

mergeListItems :: [Blocks] -> [Blocks]
mergeListItems = reverse . mergeListItems' . reverse
  where
    mergeListItems' []       = []
    mergeListItems' [x]      = [x]
    mergeListItems' (x:y:zs) =
      case B.toList x of
        (BulletList _ : _ )    -> mergeListItems' $ (y <> x) : zs
        (OrderedList _ _ : _ ) -> mergeListItems' $ (y <> x) : zs
        _                      -> x : mergeListItems' (y : zs)

data ListMarker = BulletListMarker | OrderedListMarker

buildList :: ListMarker -> [Blocks] -> Blocks
buildList BulletListMarker  = B.bulletList
buildList OrderedListMarker = B.orderedList

buildListFromItems :: [(ListMarker, Blocks)] -> Blocks
buildListFromItems []                  = mempty
buildListFromItems items@((mark, _):_) = buildList mark $ mergeListItems $ map snd items
