{-# LANGUAGE OverloadedStrings #-}

module PandocFilterGraphviz where

import Crypto.Hash
import Control.Monad (unless)

import Data.ByteString (ByteString)
import Data.Byteable (toBytes)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Base16 as B16
import Data.Char(isSpace)

import qualified Data.Map.Strict as M
import Data.Maybe(isNothing)
import Data.Text as T
import qualified Data.Text.IO as TextIO
import Data.Text.Encoding as E

import System.FilePath
import System.Directory
import System.Exit
import System.Process (system)

import Text.Pandoc
import Text.Pandoc.JSON

--import qualified Debug.Trace as Trace

data Renderer = Dot | Neato | Twopi | Circo | FDP | SFDP | Patchwork
instance Show Renderer where
  show Dot = "dot"
  show Neato = "neato"
  show Twopi = "twopi"
  show Circo = "circo"
  show FDP = "fdp"
  show SFDP = "sfdp"
  show Patchwork = "patchwork"

rendererFromString :: Text -> Maybe Renderer
rendererFromString "dot" = Just Dot
rendererFromString "neato" = Just Neato
rendererFromString "twopi" = Just Twopi
rendererFromString "circo" = Just Circo
rendererFromString "fdp" = Just FDP
rendererFromString "sfdp" = Just SFDP
rendererFromString "patchwork" = Just Patchwork
rendererFromString _ = Nothing

(¤) :: Text -> Text -> Text
(¤) = T.append

hexSha3_512 :: ByteString -> ByteString
hexSha3_512 bs = C8.pack $ show (hash bs :: Digest SHA3_512)

sha :: Text -> Text
sha = E.decodeUtf8 . hexSha3_512 . B16.encode . E.encodeUtf8

fileName4Code :: Text -> Text -> Maybe Text -> FilePath
fileName4Code name source ext =
  filename
  where
    dirname = name ¤ "-images"
    shaN = sha source
    barename = shaN ¤ (case ext of
        Just e -> "." ¤ e
        Nothing -> "")
    filename = T.unpack dirname </> T.unpack barename

getCaption :: M.Map Text Text -> (Text,Text)
getCaption m = case M.lookup "caption" m of
  Just cap -> (cap,"fig:")
  Nothing -> ("","")

getFmt :: Maybe Format -> String
getFmt mfmt = case mfmt of
  Just (Format "latex") -> "pdf"
  Just _ -> "png"
  Nothing -> "png"

renderDot1 :: Maybe Format -> Maybe Renderer -> FilePath -> IO FilePath
renderDot1 mfmt mrndr src = renderDot mfmt rndr src dst >> return dst
  where
    dst = (dropExtension src) <.> (getFmt mfmt)
    rndr = case mrndr of
      Just r -> r
      Nothing -> Dot

renderDot :: Maybe Format -> Renderer -> FilePath -> FilePath -> IO ExitCode
renderDot mfmt rndr src dst =
  system $
    Prelude.unwords [ show rndr
                    , "-T" ++ (getFmt mfmt)
                    , "-o" ++ show dst
                    , show src ]

graphviz :: Maybe Format -> Block -> IO Block
graphviz mfmt cblock@(CodeBlock (id, classes, attrs) content) =
    if isDot id || Prelude.any isDot classes then do
      ensureFile dest
      TextIO.writeFile dest content
      img <- renderDot1 mfmt mrndr dest
      ensureFile img
      return $ Para [Image (id,classes,attrs) [] (T.pack img, caption)]
    else return cblock
  where
    dotContent = if isEmptyText content
                    then emptyDiagram
                    else content
    isDot = ("dot"==) . T.toLower
    dest = fileName4Code "graphviz" content (Just "dot")
    ensureFile fp =
      let dir = takeDirectory fp
      in do
        createDirectoryIfMissing True dir
        exist <- doesFileExist fp
        unless exist $ TextIO.writeFile fp emptyDiagram
    --toTextPairs = Prelude.map (\(f,s) -> (T.pack f,T.pack s))
    m = M.fromList $ attrs
    mrndr = case M.lookup "renderer" m of
      Just str -> rendererFromString str
      _ -> Nothing
    (caption, typedef) = getCaption m
graphviz fmt x = return x

isEmptyText :: Text -> Bool
isEmptyText = isNothing . T.find (not . isSpace)

emptyDiagram :: Text
emptyDiagram = "digraph EmptyDiagram { node [shape=\"plain\"]; \"Diagram is empty\"; }"

