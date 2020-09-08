{-# LANGUAGE NoImplicitPrelude, FlexibleInstances #-}
module Textable (Textable(..), Tagger(..)) where

import "tagsoup" Text.StringLike (StringLike)
import "tagsoup" Text.HTML.TagSoup (Tag, toTagRep)

import qualified "bytestring" Data.ByteString as BS
import qualified "bytestring" Data.ByteString.Char8 as BSC
import qualified "bytestring" Data.ByteString.Lazy as BL
import qualified "bytestring" Data.ByteString.Lazy.Char8 as BLC
import qualified "text" Data.Text as TS
import qualified "text" Data.Text.Lazy as TL

import Data.Bool (Bool)
import Data.Function ((.))
import Data.Char (Char, isSpace)
import Data.Maybe (Maybe)
import Data.Tuple (fst)
import Data.Monoid (Monoid)
import Data.String (IsString(fromString))

newtype Tagger a = Tag { getTag :: Tag a }

instance StringLike a => IsString (Tagger a) where
  fromString = Tag . toTagRep

class (StringLike a, Monoid a) => Textable a where
  dropWhile, dropWhileEnd, dropAround :: (Char -> Bool) -> a -> a
  reverse, strip :: a -> a
  stripPrefix, stripSuffix :: a -> a -> Maybe a
  span :: (Char -> Bool) -> a -> (a, a)

  dropWhileEnd f = reverse . dropWhile f . reverse
  dropAround f = dropWhileEnd f . dropWhile f
  strip = dropAround isSpace

instance Textable TS.Text where
  dropWhile = TS.dropWhile
  dropWhileEnd = TS.dropWhileEnd
  dropAround = TS.dropAround
  reverse = TS.reverse
  strip = TS.strip
  stripPrefix = TS.stripPrefix
  stripSuffix = TS.stripSuffix
  span = TS.span

instance Textable TL.Text where
  dropWhile = TL.dropWhile
  dropWhileEnd = TL.dropWhileEnd
  dropAround = TL.dropAround
  reverse = TL.reverse
  strip = TL.strip
  stripPrefix = TL.stripPrefix
  stripSuffix = TL.stripSuffix
  span = TL.span

instance Textable BS.ByteString where
  dropWhile = BSC.dropWhile
  dropWhileEnd f = fst . BSC.spanEnd f
  reverse = BS.reverse
  stripPrefix = BS.stripPrefix
  stripSuffix = BS.stripSuffix
  span = BSC.span

instance Textable BL.ByteString where
  dropWhile = BLC.dropWhile
  reverse = BL.reverse
  stripPrefix = BL.stripPrefix
  stripSuffix = BL.stripSuffix
  span = BLC.span

