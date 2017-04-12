{-# LANGUAGE OverloadedStrings #-}
module Text.Pandoc.Writers.Hatena (writeHatena) where
import Control.Monad.State (StateT, evalStateT, gets, modify)
import Data.Default (Default (..))
import Data.List (intersperse, isPrefixOf)
import Text.Pandoc.Class (PandocMonad, report)
import Text.Pandoc.Logging
import Text.Pandoc.Definition
import Text.Pandoc.Options (WriterOptions)
import Text.Pandoc.Pretty

data WriterState = WriterState { stListMarkers :: String }

instance Default WriterState where
  def = WriterState ""

type HW = StateT WriterState

writeHatena :: PandocMonad m => WriterOptions -> Pandoc -> m String
writeHatena opts document = evalStateT (pandocToHatena opts document) def

pandocToHatena :: PandocMonad m => WriterOptions -> Pandoc -> HW m String
pandocToHatena opts (Pandoc _ blocks) = do
  main <- blockListToHatena opts blocks
  let colwidth = Nothing
  return $ render colwidth main

blockListToHatena :: PandocMonad m => WriterOptions -> [Block] -> HW m Doc
blockListToHatena opts blocks = vcat <$> mapM (blockToHatena opts) blocks

blockToHatena :: PandocMonad m => WriterOptions -> Block -> HW m Doc

blockToHatena _ Null = return empty

blockToHatena opts (Plain inlines) = do
  inlineListToHatena opts inlines

blockToHatena opts (Header level _ inlines) = do
  content <- inlineListToHatena opts inlines
  return $ text (take level $ repeat '*') <> " " <> content <> blankline

blockToHatena _ (CodeBlock (_, classes, _) str) = do
  let lang = case classes of
                  []  -> ""
                  l:_ -> l
  return $ blankline <> text (">|" ++ lang ++ "|") <> cr <> text str <> cr <> text "||<" <> blankline

blockToHatena opts (Para inlines) = do
  content <- inlineListToHatena opts inlines
  return $ content <> cr

blockToHatena opts (BlockQuote blocks) = do
  content <- blockListToHatena opts blocks
  return $ blankline <> ">>" <> cr <> content <> cr <> "<<" <> blankline

blockToHatena opts (BulletList lst) = generalList "-" opts lst

blockToHatena opts (OrderedList _ lst) = generalList "+" opts lst

blockToHatena opts (DefinitionList lst) = do
  items <- mapM renderItem lst
  return $ blankline <> mconcat items <> blankline
  where
    renderItem (term, dfn) = do
      term' <- inlineListToHatena opts term
      dfn' <- mconcat <$> mapM (blockListToHatena opts) dfn
      return $ ":" <> term' <> ":" <> dfn' <> cr

blockToHatena opts (Div _ lst) = do
  content <- blockListToHatena opts lst
  return $ "><div>" <> cr <> content <> cr <> "</div><" <> blankline

blockToHatena _ HorizontalRule = do
  return $ blankline <> "><hr><" <> blankline

blockToHatena _ (RawBlock (Format "html") tag) = do
  return $ text tag

blockToHatena _ b@(RawBlock _ _) = do
  report $ BlockNotRendered b
  return $ empty

blockToHatena opts (LineBlock inlines) = do
  mconcat <$> mapM (inlineListToHatena opts) inlines

-- TODO caption
blockToHatena opts (Table _ _ _ header rows) = do
  header' <- catRow "|*" header
  rows' <- vcat <$> map (\row -> "|" <> row <> "|") <$> mapM (catRow "|") rows
  return $ blankline <> (if all null header then empty else "|*" <> header' <> "|" <> cr) <> rows' <> blankline
  where
    catRow sep row = mconcat <$> intersperse (text sep) <$> mapM (blockListToHatena opts) row

generalList :: PandocMonad m => String -> WriterOptions -> [[Block]] -> HW m Doc
generalList marker opts lst = do
  markers <- gets stListMarkers
  items <- mapM (renderItem markers) lst
  return $ (if null markers then blankline else empty) <> mconcat items <> blankline
  where
    renderItem markers item = do
      modify $ \st -> st { stListMarkers = markers ++ marker }
      d <- blockListToHatena opts item
      modify $ \st -> st { stListMarkers = markers }
      return $ text (markers ++ marker ++ " ") <> d <> cr

inlineListToHatena :: PandocMonad m => WriterOptions -> [Inline] -> HW m Doc
inlineListToHatena opts lst = do
  go lst
  where go [] = return empty
        go (x:xs) = do
          x' <- inlineToHatena opts x
          xs' <- go xs
          return $ x' <> xs'

inlineToHatena :: PandocMonad m => WriterOptions -> Inline -> HW m Doc
inlineToHatena opts (Emph lst) = surroundTag "em" opts lst

inlineToHatena opts (Strong lst) = surroundTag "strong" opts lst

inlineToHatena opts (Subscript lst) = surroundTag "sub" opts lst

inlineToHatena opts (Superscript lst) = surroundTag "sup" opts lst

inlineToHatena opts (Strikeout lst) = surroundTag "s" opts lst

inlineToHatena opts (Cite _ lst) = surroundTag "cite" opts lst

inlineToHatena opts (Span _ lst) = surroundTag "span" opts lst

inlineToHatena opts (Quoted DoubleQuote lst) = do
  contents <- inlineListToHatena opts lst
  return $ "\"" <> contents <> "\""

inlineToHatena opts (Quoted SingleQuote lst) = do
  contents <- inlineListToHatena opts lst
  return $ "'" <> contents <> "'"

inlineToHatena _ (Code _ str) = do
  return $ "<code>" <> text str <> "</code>"

inlineToHatena _ (Math InlineMath str) = do
  return $ "[tex:" <> text str <> "]"

inlineToHatena _ m@(Math DisplayMath _) = do
  report $ InlineNotRendered m
  return empty

inlineToHatena _ (Str str) = return $ text str

inlineToHatena opts (Link _ txt (uri, _)) = do
  txt' <- inlineListToHatena opts txt
  if isPrefixOf "http://" uri || isPrefixOf "https://" uri
    then return $ "[" <> text uri <> ":title=" <> txt' <> "]"
    else return $ "<a href=\"" <> text uri <> "\">" <> txt' <> "</a>"

inlineToHatena _ (Image _ _ (uri, _)) = do
  if isPrefixOf "http://" uri || isPrefixOf "https://" uri
    then return $ "[" <> text uri <> ":image]"
    else return $ "<img src=\"" <> text uri <> "\">"

inlineToHatena opts (SmallCaps lst) = inlineListToHatena opts lst

inlineToHatena opts (Note lst) = do
  contents <- blockListToHatena opts lst
  return $ "((" <> contents <> "))" -- FIXME should be inline

inlineToHatena _ Space = return " "

inlineToHatena _ SoftBreak = return " "

inlineToHatena _ LineBreak = return "<br>"

inlineToHatena _ il@(RawInline f str)
  | f == Format "tex"  = return $ "[tex:" <> text str <> "]"
  | f == Format "html" = return $ text str
  | otherwise          = empty <$ report (InlineNotRendered il)

surroundTag :: PandocMonad m => String -> WriterOptions -> [Inline] -> HW m Doc
surroundTag tag opts lst = do
  contents <- inlineListToHatena opts lst
  return $ text ("<"++tag++">") <> contents <> text ("</"++tag++">")
