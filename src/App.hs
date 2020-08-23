{-# LANGUAGE OverloadedStrings #-}
module App (main) where

import Request
import Helper

import "tagsoup" Text.HTML.TagSoup (Tag)
import Data.List (lookup)

main :: IO ()
main = do
  html <- getProblem 19548
  -- writeFileLBS "19548.html" html
  -- html <- readFileLText "19548.html"
  maybe (return ()) id . fmap print . evalStateT parse $ parseTags html

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

  let samples = listup getSample sampleText

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
    no <- lift $ lookup "id" attr >>= stripPrefix prefix
    (no, ) <$> getText

getSample :: Act (Sample LText)
getSample = do
  (inputNo, inputText) <- getDeadSample "sample-input-"
  (outputNo, outputText) <- getDeadSample "sample-output-"

  if inputNo == outputNo
    then pure $ Sample (inputText, outputText)
    else lift Nothing
