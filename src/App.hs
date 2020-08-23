{-# LANGUAGE OverloadedStrings #-}

module App where

import Network.HTTP.Req
import Text.HTML.TagSoup ((~==), (~/=), Tag(..), parseTagsOptions, parseOptionsFast, isTagText)
import Data.List (span, lookup)
import qualified Data.Text.Lazy as L

parseTags :: LText -> [Tag LText]
parseTags = parseTagsOptions parseOptionsFast

main :: IO ()
main = do
  html <- acmicpc 19548
  -- writeFileLBS "19548.html" html
  -- html <- readFileLText "19548.html"
  maybe (return ()) id . fmap print . evalStateT parse $ parseTags html

toMaybe :: (a -> Bool) -> Maybe a -> Maybe a
toMaybe f x@(Just a)
  | f a = x
  | otherwise = Nothing
toMaybe _ _ = Nothing

acmicpc :: Word32 -> IO LText
acmicpc problem = runReq defaultHttpConfig $ do
  r <- req GET (https "www.acmicpc.net" /: "problem" /: show problem) NoReqBody lbsResponse mempty
  pure . decodeUtf8 $ responseBody r

type St = [Tag LText]
type Ac = StateT St
type Act = Ac Maybe

dropTill :: String -> Act [(LText, LText)]
dropTill str = do
  modify $ dropWhile (~/= str)

  maySep <- toMaybe (~== str) <$> gets (viaNonEmpty head)
  sep <- lift $ maySep
  modify $ drop 1

  case sep of
    TagOpen _ attr -> pure attr
    TagClose _ -> pure []
    _ -> lift $ Nothing


after :: String -> Act ()
after = void . dropTill

takeTill :: Monad m => String -> Ac m [Tag LText]
takeTill str = state $ span (~/= str)

getText :: Monad m => Ac m LText
getText = state $ collect mempty . dropWhile (not . isTagText) where
  collect text (TagText a : xs) = collect (text <> a) xs
  collect text xs = (L.strip text, xs)

getTextAfter :: String -> Act LText
getTextAfter tag = after tag *> getText

parse :: Act (Problem LText)
parse = do
  title <- getTextAfter "<span id=problem_title>"

  after "<table id=problem-info>"
  after "<tbody>"

  timeLimit <- getTextAfter "<td>"
  memoryLimit <- getTextAfter "<td>"

  after "<div id=problem-body>"
  after "<div id=problem_description>"
  description <- takeTill "</section>"

  after "<div id=problem_input>"
  input <- takeTill "</section>"

  after "<div id=problem_output>"
  output <- takeTill "</section>"

  sampleText <- takeTill "<section id=source>"

  let samples = getSamples sampleText

  pure $ Problem title timeLimit memoryLimit description input output samples

data Problem a = Problem
  { title :: a
  , timeLimit :: a
  , memoryLimit :: a
  , description :: [Tag a]
  , inputCondition :: [Tag a]
  , outputCondition :: [Tag a]
  , samples :: [Sample a]
  } deriving Show

newtype Sample a = Sample (a, a) deriving Show

getDeadSample :: LText -> Act (LText, LText)
getDeadSample prefix = do
    attr <- dropTill "<pre class=sampledata>"
    no <- lift $ lookup "id" attr >>= L.stripPrefix prefix
    body <- getText
    pure (no, body)

getSample :: Act (Sample LText)
getSample = do
  (inputNo, inputText) <- getDeadSample "sample-input-"
  (outputNo, outputText) <- getDeadSample "sample-output-"
  if inputNo == outputNo
    then pure $ Sample (inputText, outputText)
    else lift Nothing

getSamples :: St -> [Sample LText]
getSamples s =
  case runStateT getSample s of
    Just (a, s') -> a : getSamples s'
    Nothing -> []
