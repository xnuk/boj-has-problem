{-# LANGUAGE OverloadedStrings #-}
module App (main) where

-- import Request
import Helper
import Q (q)

import "tagsoup" Text.HTML.TagSoup (Tag, innerText)
import Data.List (lookup)

import Textable (Textable)

main :: IO ()
main = do
  -- html <- getProblem 19548
  -- writeFileLBS "19548.html" html
  html <- readFileLText "19548.html"
  maybe (return ()) id . fmap (putLText . showProblem) . evalStateT parse $ parseTags html

parse :: AcmskM Problem
parse = do
  number <- getProblemNumber
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

  pure $ Problem number title timeLimit memoryLimit description input output samples

data Problem a = Problem
  { number :: a
  , title :: a
  , timeLimit :: a
  , memoryLimit :: a
  , description :: [Tag a]
  , inputCondition :: [Tag a]
  , outputCondition :: [Tag a]
  , samples :: [Sample a]
  }

newtype Sample a = Sample (a, a) deriving Show

getProblemNumber :: AcmskM Id
getProblemNumber = do
  attr <- dropTill "<meta name=problem-id>"
  lift $ lookup "content" attr

getDeadSample :: Textable a => a -> AcsM a (a, a)
getDeadSample prefix = do
    attr <- dropTill "<pre class=sampledata>"
    no <- lift $ lookup "id" attr >>= stripPrefix prefix
    (no, ) <$> getText

getSample :: AcmskM Sample
getSample = do
  (inputNo, inputText) <- getDeadSample "sample-input-"
  (outputNo, outputText) <- getDeadSample "sample-output-"

  if inputNo == outputNo
    then pure $ Sample (inputText, outputText)
    else lift Nothing

showProblem :: Textable a => Problem a -> a
showProblem Problem{..} = [q|-$
  ---
  번호: ${number}
  시간 제한: ${timeLimit}
  메모리 제한: ${memoryLimit}
  ---

  # ${title}
  ${innerText description}

  ## 입력
  ${innerText inputCondition}

  ## 출력
  ${innerText outputCondition}
  |]
