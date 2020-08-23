{-# LANGUAGE OverloadedStrings #-}
module Request (getProblem) where

import "req" Network.HTTP.Req

getProblem :: Word32 -> IO LText
getProblem problem =
  runReq defaultHttpConfig $
    (decodeUtf8 . responseBody) <$>
      req GET url NoReqBody lbsResponse mempty
  where
    url = https "www.acmicpc.net" /: "problem" /: show problem
