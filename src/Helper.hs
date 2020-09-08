module Helper
  ( Acmsk, AcmskM, AcmsM, Acsk, Id, Acs, AcsM, parseTags
  , takeTill, dropTill, after
  , getText, getTextAfter
  , strip, stripPrefix
  , listup
  ) where

import "tagsoup" Text.HTML.TagSoup
  ( Tag(..)
  , isTagText, parseTagsOptions, parseOptionsFast
  )

import qualified "tagsoup" Text.HTML.TagSoup as T

import Data.List (span)
import Textable (Textable, strip, stripPrefix, Tagger(..))

type Stt a = [Tag a]
type Attr a = [(a, a)]
type Constant b a = b
type Id a = a
type Acs a b = forall m. Monad m => StateT (Stt a) m b
type AcsM a b = StateT (Stt a) Maybe b

type StateMachine a m k = StateT (Stt a) m (k a)
type StateMachineConstant a m b = StateMachine a m (Constant b)
type Acmsk m k = forall a. (Textable a) => StateMachine a m k
type Acms m b = Acmsk m (Constant b)
type Acsk k = forall m. Monad m => Acmsk m k
type AcmskM k = Acmsk Maybe k
type AcmsM b = Acms Maybe b

(~/=), (~==) :: Textable a => Tag a -> Tagger a -> Bool
a ~/= (Tag b) = (T.~/=) a b
a ~== (Tag b) = (T.~==) a b

parseTags :: Textable a => a -> [Tag a]
parseTags = parseTagsOptions parseOptionsFast

toMaybe :: (a -> Bool) -> Maybe a -> Maybe a
toMaybe f x@(Just a)
  | f a = x
  | otherwise = Nothing
toMaybe _ _ = Nothing

takeTill :: Textable a => Tagger a -> Acs a (Stt a)
takeTill str = state $ span (~/= str)

dropTill :: Textable a => Tagger a -> AcsM a (Attr a)
dropTill str = do
  modify $ dropWhile (~/= str)

  maySep <- toMaybe (~== str) <$> gets (viaNonEmpty head)
  sep <- lift $ maySep
  modify $ drop 1

  case sep of
    TagOpen _ attr -> pure attr
    TagClose _ -> pure []
    _ -> lift $ Nothing

after :: Textable a => Tagger a -> AcsM a ()
after = void . dropTill

getTextWith :: Monoid a => (a -> a) -> Acs a a
getTextWith f = state $ collect mempty . dropWhile (not . isTagText) where
  collect text (TagText a : xs) = collect (text <> a) xs
  collect text xs = (f text, xs)

getText :: Acsk Id
getText = getTextWith strip

getTextAfter :: Textable a => Tagger a -> AcsM a a
getTextAfter tag = after tag *> getText

listup :: StateMachineConstant a Maybe b -> Stt a -> [b]
listup act = runner <&> \case
  Just (a, s) -> a : listup act s
  Nothing -> []
  where
    runner = runStateT act
