module Helper
  ( Act
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
import "tagsoup" Text.StringLike (StringLike)

import Data.List (span)
import "text" Data.Text.Lazy (strip, stripPrefix)

type Tx = LText
type Stt a = [Tag a]
type Attr a = [(a, a)]
type ConstVoid a = ()
type Acs a b = forall m. Monad m => StateT (Stt a) m b
type ActMaybe a = StateT (Stt a) Maybe
type Act = ActMaybe Tx

type StateMachine a m k = StateT (Stt a) m (k a)
type Acmsk m k = forall a. StringLike a => StateMachine a m k
type Acsk k = forall m. Monad m => Acmsk m k

parseTags :: Tx -> [Tag Tx]
parseTags = parseTagsOptions parseOptionsFast

toMaybe :: (a -> Bool) -> Maybe a -> Maybe a
toMaybe f x@(Just a)
  | f a = x
  | otherwise = Nothing
toMaybe _ _ = Nothing

takeTill :: String -> Acsk Stt
takeTill str = state $ span (~/= str)

dropTill :: String -> Acmsk Maybe Attr
dropTill str = do
  modify $ dropWhile (~/= str)

  maySep <- toMaybe (~== str) <$> gets (viaNonEmpty head)
  sep <- lift $ maySep
  modify $ drop 1

  case sep of
    TagOpen _ attr -> pure attr
    TagClose _ -> pure []
    _ -> lift $ Nothing

after :: String -> Acmsk Maybe ConstVoid
after = void . dropTill

getTextWith :: Monoid a => (a -> a) -> Acs a a
getTextWith f = state $ collect mempty . dropWhile (not . isTagText) where
  collect text (TagText a : xs) = collect (text <> a) xs
  collect text xs = (f text, xs)

getText :: Acs Tx Tx
getText = getTextWith strip

getTextAfter :: String -> ActMaybe Tx Tx
getTextAfter tag = after tag *> getText

listup :: ActMaybe a b -> Stt a -> [b]
listup act = runner <&> \case
  Just (a, s) -> a : listup act s
  Nothing -> []
  where
    runner = runStateT act
