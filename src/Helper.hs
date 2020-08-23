module Helper
  ( St, Ac, Act
  , parseTags
  , takeTill, dropTill, after
  , getText, getTextAfter
  , strip, stripPrefix
  , listup
  ) where

import "tagsoup" Text.HTML.TagSoup
  ( Tag(..)
  , (~/=), (~==)
  , isTagText, parseTagsOptions, parseOptionsFast
  )

import Data.List (span)
import "text" Data.Text.Lazy (strip, stripPrefix)

type St = [Tag LText]
type Ac = StateT St
type Act = Ac Maybe

parseTags :: LText -> [Tag LText]
parseTags = parseTagsOptions parseOptionsFast

toMaybe :: (a -> Bool) -> Maybe a -> Maybe a
toMaybe f x@(Just a)
  | f a = x
  | otherwise = Nothing
toMaybe _ _ = Nothing

takeTill :: Monad m => String -> Ac m [Tag LText]
takeTill str = state $ span (~/= str)

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

getText :: Monad m => Ac m LText
getText = state $ collect mempty . dropWhile (not . isTagText) where
  collect text (TagText a : xs) = collect (text <> a) xs
  collect text xs = (strip text, xs)

getTextAfter :: String -> Act LText
getTextAfter tag = after tag *> getText

listup :: Act a -> St -> [a]
listup act = runner <&> \case
  Just (a, s) -> a : listup act s
  Nothing -> []
  where
    runner = runStateT act
