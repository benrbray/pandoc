{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Readers.RTF
   Copyright   : Copyright (C) 2021 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane (<jgm@berkeley.edu>)
   Stability   : alpha
   Portability : portable

Conversion of RTF documents 'Pandoc' document.
-}
module Text.Pandoc.Readers.RTF (readRTF) where

import qualified Data.IntMap as IntMap
import Control.Monad
import Control.Monad.Except (throwError)
import Data.List (find, foldl')
import Data.Default
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Text.Pandoc.Builder (Blocks, Inlines)
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Class.PandocMonad (PandocMonad (..), insertMedia)
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Parsing
import Text.Pandoc.Shared (safeRead, tshow)
import Data.Char (isAlphaNum, chr, digitToInt)
import qualified Data.ByteString.Lazy as BL
import Data.Digest.Pure.SHA (sha1, showDigest)
import Data.Maybe (mapMaybe)
import Safe (lastMay, initSafe)
import Debug.Trace

{-
TODO:
- pard etc (reset to style 0)
- lists
- tables
- block quotes
- section headers
-}

-- | Read RTF from an input string and return a Pandoc document.
readRTF  :: (PandocMonad m, ToSources a)
         => ReaderOptions
         -> a
         -> m Pandoc
readRTF opts s = do
  let sources = toSources s
  parsed <- readWithM parseRTF def{ sOptions = opts } sources
  case parsed of
       Left e  -> throwError e
       Right d -> return d

data RTFState = RTFState  { sOptions     :: ReaderOptions
                          , sGroupStack  :: [Properties]
                          , sTextContent :: [(Properties, Text)]
                          , sMetadata    :: [(Text, Inlines)]
                          , sFontTable   :: FontTable
                          , sStylesheet  :: Stylesheet
                          } deriving (Show)

instance Default RTFState where
 def = RTFState { sOptions = def
                , sGroupStack = []
                , sTextContent = []
                , sMetadata = []
                , sFontTable = mempty
                , sStylesheet = mempty
                }

type FontTable = IntMap.IntMap FontFamily

data FontFamily =
  Roman | Swiss | Modern | Script | Decor | Tech | Bidi
  deriving (Show, Eq)

data StyleType = ParagraphStyle | SectionStyle | CharStyle
  deriving (Show, Eq)

data Style =
  Style { styleNum :: Int
        , styleType :: StyleType
        , styleBasedOn :: Maybe Int
        , styleName :: Text
        , styleFormatting :: [Tok]
        } deriving (Show, Eq)

type Stylesheet = IntMap.IntMap Style

data PictType =
  Emfblip | Pngblip | Jpegblip
  deriving (Show, Eq)

data Pict =
  Pict { picType :: Maybe PictType
       , picWidth :: Maybe Int
       , picHeight :: Maybe Int
       , picWidthGoal :: Maybe Int
       , picHeightGoal :: Maybe Int
       , picBinary :: Bool
       , picData :: Text
       , picName :: Text
       , picBytes :: BL.ByteString
       } deriving (Show, Eq)

instance Default Pict where
 def = Pict { picType = Nothing
            , picWidth = Nothing
            , picHeight = Nothing
            , picWidthGoal = Nothing
            , picHeightGoal = Nothing
            , picBinary = False
            , picData = mempty
            , picName = mempty
            , picBytes = mempty }

data Properties =
  Properties
  { gBold :: Bool
  , gItalic :: Bool
  , gCaps :: Bool
  , gDeleted :: Bool
  , gSub :: Bool
  , gSuper :: Bool
  , gSmallCaps :: Bool
  , gUnderline :: Bool
  , gHyperlink :: Maybe Text
  , gImage :: Maybe Pict
  , gFontFamily :: Maybe FontFamily
  } deriving (Show, Eq)

instance Default Properties where
   def = Properties { gBold = False
                    , gItalic = False
                    , gCaps = False
                    , gDeleted = False
                    , gSub = False
                    , gSuper = False
                    , gSmallCaps = False
                    , gUnderline = False
                    , gHyperlink = Nothing
                    , gImage = Nothing
                    , gFontFamily = Nothing
                    }

type RTFParser m = ParserT Sources RTFState m


parseRTF :: PandocMonad m => RTFParser m Pandoc
parseRTF = do
  skipMany nl
  toks <- many tok
  -- return $! traceShowId toks
  doc <- B.doc <$> foldM processTok mempty toks
  kvs <- sMetadata <$> getState
  pure $ foldr (\(k,v) -> B.setMeta k v) doc kvs

data Tok = Tok SourcePos TokContents
  deriving (Show, Eq)

data TokContents =
    ControlWord Text (Maybe Int)
  | ControlSymbol Char
  | UnformattedText Text
  | Grouped [Tok]
  deriving (Show, Eq)

tok :: PandocMonad m => RTFParser m Tok
tok = do
  pos <- getPosition
  Tok pos <$> ((controlThing <|> unformattedText <|> grouped) <* skipMany nl)
 where
  controlThing = do
    char '\\' *>
      ( (ControlWord <$> letterSequence <*> (parameter <* optional delimChar))
     <|> (ControlSymbol <$> anyChar))
  parameter = do
    hyph <- string "-" <|> pure ""
    rest <- many digit
    let pstr = T.pack $ hyph <> rest
    return $ safeRead pstr
  letterSequence = T.pack <$> many1 (satisfy (\c -> c >= 'a' && c <= 'z'))
  unformattedText =
    UnformattedText . T.pack <$> many1 (satisfy (not . isSpecial))
  grouped = Grouped <$> (char '{' *> skipMany nl *> manyTill tok (char '}'))

nl :: PandocMonad m => RTFParser m ()
nl = void (char '\n' <|> char '\r')

isSpecial :: Char -> Bool
isSpecial '{' = True
isSpecial '}' = True
isSpecial '\\' = True
isSpecial '\n' = True
isSpecial _ = False

delimChar :: PandocMonad m => RTFParser m Char
delimChar = satisfy (\c -> not (isAlphaNum c || isSpecial c))

modifyGroup :: PandocMonad m
            => (Properties -> Properties)
            -> RTFParser m ()
modifyGroup f =
  updateState $ \st ->
    st{ sGroupStack =
          case sGroupStack st of
            [] -> []
            (x:xs) -> f x : xs }

addFormatting :: (Properties, Text) -> Inlines
addFormatting (_, "\n") = B.linebreak
addFormatting (props, txt) = -- Debug.Trace.trace (show (props, txt)) $
  (if gBold props then B.strong else id) .
  (if gItalic props then B.emph else id) .
  (if gDeleted props then B.strikeout else id) .
  (if gSub props then B.subscript else id) .
  (if gSuper props then B.superscript else id) .
  (if gSmallCaps props then B.smallcaps else id) .
  (if gUnderline props then B.underline else id) .
  (case gHyperlink props of
     Nothing -> id
     Just linkdest -> B.link linkdest mempty) .
  (case gFontFamily props of
     Just Modern -> B.code
     _ -> case gImage props of
            Just pict ->
              let attr = ("",[],
                         (case picWidthGoal pict of
                           Nothing -> []
                           Just w  -> [("width", tshow (fromIntegral w / 1440
                                                         :: Double)
                                          <> "in")]) ++
                         (case picHeightGoal pict of
                            Nothing -> []
                            Just h -> [("height", tshow (fromIntegral h / 1440
                                                         :: Double)
                                          <> "in")]))
              in  B.imageWith attr (picName pict) "" . B.text
            Nothing -> B.text) .
  (if gCaps props then T.toUpper else id)
  $ txt

addText :: PandocMonad m => Text -> RTFParser m ()
addText t = do
  gs <- sGroupStack <$> getState
  let props = case gs of
                (x:_) -> x
                _ -> def
  updateState (\s -> s{ sTextContent = (props, t) : sTextContent s })

inGroup :: PandocMonad m => RTFParser m a -> RTFParser m a
inGroup p = do
  updateState $ \st ->
    st{ sGroupStack =
        case sGroupStack st of
          [] -> [def]
          (x:xs) -> (x:x:xs) } -- inherit current group's properties
  result <- p
  updateState $ \st ->
    st{ sGroupStack =
        case sGroupStack st of
          [] -> [] -- should not happen
          (_:xs) -> xs }
  return result

getStyleFormatting :: PandocMonad m => Int -> RTFParser m [Tok]
getStyleFormatting stynum = do
  stylesheet <- sStylesheet <$> getState
  case IntMap.lookup stynum stylesheet of
    Nothing -> return []
    Just sty ->
      case styleBasedOn sty of
        Just i -> (<> styleFormatting sty)  <$> getStyleFormatting i
        Nothing -> return $ styleFormatting sty

isMetadataField :: Text -> Bool
isMetadataField "title" = True
isMetadataField "subject" = True
isMetadataField "author" = True
isMetadataField "manager" = True
isMetadataField "company" = True
isMetadataField "operator" = True
isMetadataField "category" = True
isMetadataField "keywords" = True
isMetadataField "comment" = True
isMetadataField "doccomm" = True
isMetadataField "hlinkbase" = True
isMetadataField _ = False

isHeaderFooter :: Text -> Bool
isHeaderFooter "header" = True
isHeaderFooter "headerl" = True
isHeaderFooter "headerr" = True
isHeaderFooter "headerf" = True
isHeaderFooter "footer" = True
isHeaderFooter "footerl" = True
isHeaderFooter "footerr" = True
isHeaderFooter "footerf" = True
isHeaderFooter _ = False

boolParam :: Maybe Int -> Bool
boolParam (Just 0) = False
boolParam _ = True

isUnderline :: Text -> Bool
isUnderline "ul" = True
isUnderline "uld" = True
isUnderline "uldash" = True
isUnderline "uldashd" = True
isUnderline "uldashdd" = True
isUnderline "uldb" = True
isUnderline "ulth" = True
isUnderline "ulw" = True
isUnderline "ulwave" = True
isUnderline _ = False

processTok :: PandocMonad m => Blocks -> Tok -> RTFParser m Blocks
processTok bs (Tok pos tok') = do
  setPosition pos
  case tok' of
    Grouped (Tok _ (ControlWord "fonttbl" _) : toks) -> inGroup $ do
      let tbl = processFontTable toks
      updateState $ \s -> s{ sFontTable = tbl }
      pure bs
    Grouped (Tok _ (ControlWord "field" _) : toks) ->
      inGroup $ handleField bs toks
    Grouped (Tok _ (ControlWord "pict" _) : toks) ->
      inGroup $ handlePict bs toks
    Grouped (Tok _ (ControlWord "stylesheet" _) : toks) ->
      inGroup $ handleStylesheet bs toks
    Grouped (Tok _ (ControlWord "filetbl" _) : _) -> pure bs
    Grouped (Tok _ (ControlWord "colortbl" _) : _) -> pure bs
    Grouped (Tok _ (ControlWord "expandedcolortbl" _) : _) -> pure bs
    Grouped (Tok _ (ControlWord "listtables" _) : _) -> pure bs
    Grouped (Tok _ (ControlWord "revtbl" _) : _) -> pure bs
    Grouped (Tok _ (ControlWord "bkmkstart" _) : _) -> pure bs -- TODO
    Grouped (Tok _ (ControlWord "bkmkend" _) : _) -> pure bs -- TODO
    Grouped (Tok _ (ControlWord f _) : _) | isHeaderFooter f -> pure bs
    Grouped (Tok _ (ControlWord "info" _) : toks) ->
      inGroup $ foldM processTok bs toks
    Grouped (Tok _ (ControlWord f _) : toks) | isMetadataField f -> inGroup $ do
      _ <- foldM processTok mempty toks
      annotatedToks <- reverse . sTextContent <$> getState
      updateState $ \s -> s{ sTextContent = [] }
      let ils = B.trimInlines . mconcat $ map addFormatting annotatedToks
      updateState $ \s -> s{ sMetadata = (f, ils) : sMetadata s }
      pure bs
    Grouped toks -> inGroup (foldM processTok bs toks)
    UnformattedText t -> bs <$ addText t
    ControlSymbol '\\' -> bs <$ addText "\\"
    ControlSymbol '{' -> bs <$ addText "{"
    ControlSymbol '}' -> bs <$ addText "}"
    ControlSymbol '~' -> bs <$ addText "\x00a0"
    ControlSymbol '-' -> bs <$ addText "\x00ad"
    ControlSymbol '_' -> bs <$ addText "\x2011"
    ControlWord "lquote" _ -> bs <$ addText "\x2018"
    ControlWord "rquote" _ -> bs <$ addText "\x2019"
    ControlWord "ldblquote" _ -> bs <$ addText "\x201C"
    ControlWord "rdblquote" _ -> bs <$ addText "\x201D"
    ControlWord "emdash" _ -> bs <$ addText "\x2014"
    ControlWord "emspace" _ -> bs <$ addText "\x2003"
    ControlWord "enspace" _ -> bs <$ addText "\x2002"
    ControlWord "endash" _ -> bs <$ addText "\x2013"
    ControlWord "bullet" _ -> bs <$ addText "\x2022"
    ControlWord "tab" _ -> bs <$ addText "\t"
    ControlWord "line" _ -> bs <$ addText "\n"
    ControlWord "cs" (Just n) -> do
      getStyleFormatting n >>= foldM processTok bs
    ControlWord "s" (Just n) -> do
      getStyleFormatting n >>= foldM processTok bs
    ControlWord "ds" (Just n) -> do
      getStyleFormatting n >>= foldM processTok bs
    ControlWord "f" (Just i) -> bs <$ do
      fontTable <- sFontTable <$> getState
      modifyGroup (\g -> g{ gFontFamily = IntMap.lookup i fontTable })
    ControlWord "u" (Just i) -> bs <$ addText (T.singleton (chr i))
    ControlWord "caps" mbp -> bs <$
      modifyGroup (\g -> g{ gCaps = boolParam mbp })
    ControlWord "deleted" mbp -> bs <$
      modifyGroup (\g -> g{ gDeleted = boolParam mbp })
    ControlWord "b" mbp -> bs <$
      modifyGroup (\g -> g{ gBold = boolParam mbp })
    ControlWord "i" mbp -> bs <$
      modifyGroup (\g -> g{ gItalic = boolParam mbp })
    ControlWord "sub" mbp -> bs <$
      modifyGroup (\g -> g{ gSub = boolParam mbp })
    ControlWord "super" mbp -> bs <$
      modifyGroup (\g -> g{ gSuper = boolParam mbp })
    ControlWord "up" mbp -> bs <$
      modifyGroup (\g -> g{ gSuper = boolParam mbp })
    ControlWord "strike" mbp -> bs <$
      modifyGroup (\g -> g{ gDeleted = boolParam mbp })
    ControlWord "strikedl" mbp -> bs <$
      modifyGroup (\g -> g{ gDeleted = boolParam mbp })
    ControlWord "scaps" mbp -> bs <$
      modifyGroup (\g -> g{ gSmallCaps = boolParam mbp })
    ControlWord x mbp | isUnderline x -> bs <$
      modifyGroup (\g -> g{ gUnderline = boolParam mbp })
    ControlWord "ulnone" _ -> bs <$
      modifyGroup (\g -> g{ gUnderline = False })
    ControlWord "pard" _ -> bs <$ do
      modifyGroup (const def)
      stylesheet <- sStylesheet <$> getState
      case IntMap.lookup 0 stylesheet of
        Nothing -> pure bs
        Just sty -> foldM processTok bs (styleFormatting sty)
    ControlWord "par" _ -> do
      annotatedToks <- reverse . sTextContent <$> getState
      updateState $ \s -> s{ sTextContent = [] }
      let justCode = def{ gFontFamily = Just Modern }
      return $ bs <>
        if null annotatedToks
           then mempty
           else if all ((== justCode) . fst) annotatedToks
                then B.codeBlock (mconcat $ map snd annotatedToks)
                else B.para $ B.trimInlines . mconcat
                            $ map addFormatting annotatedToks
    _ -> pure bs

-- {\field{\*\fldinst{HYPERLINK "http://pandoc.org"}}{\fldrslt foo}}
handleField :: PandocMonad m => Blocks -> [Tok] -> RTFParser m Blocks
handleField bs
  (Tok _
    (Grouped [Tok _ (ControlSymbol '*')
     ,Tok _ (ControlWord "fldinst" Nothing)
     ,Tok _ (Grouped [Tok _ (UnformattedText insttext)])])
  :linktoks)
  | Just linkdest <- getHyperlink insttext
    = do modifyGroup $ \g -> g{ gHyperlink = Just linkdest }
         result <- foldM processTok bs linktoks
         modifyGroup $ \g -> g{ gHyperlink = Nothing }
         return result
handleField bs _ = pure bs

handleStylesheet :: PandocMonad m => Blocks -> [Tok] -> RTFParser m Blocks
handleStylesheet bs toks = do
  let styles = mapMaybe parseStyle toks
  updateState $ \s -> s{ sStylesheet = IntMap.fromList
                                     $ zip (map styleNum styles) styles }
  pure bs

parseStyle :: Tok -> Maybe Style
parseStyle (Tok _ (Grouped toks)) = do
  let (styType, styNum, rest) =
        case toks of
          Tok _ (ControlWord "s" (Just n)) : ts -> (ParagraphStyle, n, ts)
          Tok _ (ControlWord "ds" (Just n)) : ts -> (SectionStyle, n, ts)
          Tok _ (ControlSymbol '*') : Tok _ (ControlWord "cs" (Just n)) : ts
                                                -> (CharStyle, n, ts)
          _ -> (ParagraphStyle, 0, toks)
  let styName = case lastMay rest of
                  Just (Tok _ (UnformattedText t)) -> T.dropWhileEnd (==';') t
                  _ -> mempty
  let isBasedOn (Tok _ (ControlWord "sbasedon" (Just _))) = True
      isBasedOn _ = False
  let styBasedOn = case find isBasedOn toks of
                     Just (Tok _ (ControlWord "sbasedon" (Just i))) -> Just i
                     _ -> Nothing
  let isStyleControl (Tok _ (ControlWord x _)) =
         x `elem` ["cs", "s", "ds", "additive", "sbasedon", "snext",
                   "sautoupd", "shidden", "keycode", "alt", "shift",
                   "ctrl", "fn"]
      isStyleControl _ = False
  let styFormatting = filter (not . isStyleControl) (initSafe rest)
  return $ Style{ styleNum = styNum
                , styleType = styType
                , styleBasedOn = styBasedOn
                , styleName = styName
                , styleFormatting = styFormatting
                }
parseStyle _ = Nothing

handlePict :: PandocMonad m => Blocks -> [Tok] -> RTFParser m Blocks
handlePict bs toks = do
  let pict = foldl' getPictData def toks
  let altText = "image"
  let binToWord = T.foldl' (\acc x -> acc * 2 + fromIntegral (digitToInt x)) 0
  let hexToWord t = case TR.hexadecimal t of
                      Left _ -> 0
                      Right (x,_) -> x
  let isBinaryDigit '0' = True
      isBinaryDigit '1' = True
      isBinaryDigit _   = False
  let bytes = BL.pack $
              if picBinary pict && T.all isBinaryDigit (picData pict)
                 then map binToWord $ T.chunksOf 8 $ picData pict
                 else map hexToWord $ T.chunksOf 2 $ picData pict
  let (mimetype, ext) =
        case picType pict of
          Just Emfblip -> (Just "image/x-emf", ".emf")
          Just Pngblip -> (Just "image/png", ".png")
          Just Jpegblip -> (Just "image/jpeg", ".jpg")
          Nothing -> (Nothing, "")
  case mimetype of
    Just mt -> do
      let pictname = showDigest (sha1 bytes) <> ext
      insertMedia pictname (Just mt) bytes
      modifyGroup $ \g -> g{ gImage = Just pict{ picName = T.pack pictname,
                                                 picBytes = bytes } }
      addText altText
      modifyGroup $ \g -> g{ gImage = Nothing }
    _ -> return ()
  return bs
 where
  getPictData :: Pict -> Tok -> Pict
  getPictData pict (Tok _ tok') =
    case tok' of
      ControlWord "emfblip" _-> pict{ picType = Just Emfblip }
      ControlWord "pngblip" _-> pict{ picType = Just Pngblip }
      ControlWord "jpegblip" _-> pict{ picType = Just Jpegblip }
      ControlWord "picw" (Just w) -> pict{ picWidth = Just w }
      ControlWord "pich" (Just h) -> pict{ picHeight = Just h }
      ControlWord "picwgoal" (Just w) -> pict{ picWidthGoal = Just w }
      ControlWord "pichgoal" (Just h) -> pict{ picHeightGoal = Just h }
      ControlWord "bin" _ -> pict{ picBinary = True }
      UnformattedText t -> pict{ picData = t }
      _ -> pict



getHyperlink :: Text -> Maybe Text
getHyperlink t =
  case T.stripPrefix "HYPERLINK " t of
    Nothing -> Nothing
    Just rest -> Just $ T.drop 1 $ T.dropEnd 1 rest -- strip " from beg and end

processFontTable :: [Tok] -> FontTable
processFontTable = snd . foldl' go (0, mempty)
 where
  go (fontnum, tbl) (Tok _ tok') =
    case tok' of
     (ControlWord "f" (Just i)) -> (i, tbl)
     (ControlWord "fnil" _) -> (fontnum, tbl)
     (ControlWord "froman" _) -> (fontnum, IntMap.insert fontnum Roman tbl)
     (ControlWord "fswiss" _) -> (fontnum, IntMap.insert fontnum Swiss tbl)
     (ControlWord "fmodern" _) -> (fontnum, IntMap.insert fontnum Modern tbl)
     (ControlWord "fscript" _) -> (fontnum, IntMap.insert fontnum Script tbl)
     (ControlWord "fdecor" _) -> (fontnum, IntMap.insert fontnum Decor tbl)
     (ControlWord "ftech" _) -> (fontnum, IntMap.insert fontnum Tech tbl)
     (ControlWord "fbidi" _) -> (fontnum, IntMap.insert fontnum Bidi tbl)
     (Grouped ts) -> foldl' go (fontnum, tbl) ts
     _ -> (fontnum, tbl)

