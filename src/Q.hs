{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Q (q) where
  
import "template-haskell" Language.Haskell.TH
import "template-haskell" Language.Haskell.TH.Quote (QuasiQuoter(..))
import "haskell-src-meta" Language.Haskell.Meta.Parse (parseExp)
import Data.List (foldr1, minimum, span)
import Data.List.NonEmpty ((<|))

notSupported :: a -> b
notSupported _ =
  error "Using other than expressions is not supported"

data Parsed = Literal String | Expression String
data NewlineEnd = Clip | Strip | Keep

data Config = Config
  { startChar :: Char
  , newlineEnd :: NewlineEnd
  }

defaultConfig :: Config
defaultConfig = Config
  { startChar = '#'
  , newlineEnd = Clip
  }

splitLn :: String -> NonEmpty String
splitLn str = case rest of
  ('\n':x) -> before <| splitLn x
  _ -> before :| []
  where (before, rest) = break (== '\n') str

isIndentChar :: Char -> Bool
isIndentChar c = c == '\t' || c == ' '

trimIndent :: [String] -> [String]
trimIndent [] = []
trimIndent xs = map (drop minIndent) xs
  where
    minIndent = minimum
      . map (length . fst)
      . filter (not . null . snd)
      . map (span isIndentChar)
      $ xs

trimNewlineEnd :: NewlineEnd -> [String] -> [String]
trimNewlineEnd Keep = id
trimNewlineEnd Strip = reverse . dropWhile null . reverse
trimNewlineEnd Clip = reverse . ("":) . dropWhile null . reverse

joinLines :: [String] -> String
joinLines = intercalate "\n"

trimSpaces :: NewlineEnd -> [String] -> String
trimSpaces a = joinLines . trimNewlineEnd a . trimIndent

detectConfig :: String -> (Config, String)
detectConfig xs
  | null restLines = (defaultConfig, firstLine)
  | otherwise = result

  where
    (firstLine :| restLines) = splitLn xs

    newlineAndChar = case firstLine of
      ""  -> Just (Clip,  Nothing)
      "+" -> Just (Keep,  Nothing)
      "-" -> Just (Strip, Nothing)

      (    c:[]) -> Just (Clip,  Just c)
      ('+':c:[]) -> Just (Keep,  Just c)
      ('-':c:[]) -> Just (Strip, Just c)

      _ -> Nothing

    result = case newlineAndChar of
      Nothing ->
        (defaultConfig, joinLines $ firstLine : restLines)

      Just (a, Nothing) ->
        ( defaultConfig { newlineEnd = a }
        , trimSpaces a restLines
        )

      Just (a, Just c) ->
        ( defaultConfig { newlineEnd = a, startChar = c }
        , trimSpaces a restLines
        )

parse :: Config -> String -> [Parsed]
parse Config{..} = go "" where
  go st "" = [Literal st]
  go st (c:'{':xs)
    | c == startChar = Literal st : Expression before : go "" after
    | otherwise = go (st <> (c:'{':"")) xs
    where
      (before, rest) = break (== '}') xs
      after = drop 1 rest

  go st xs = go (st <> before) after where
    (before, after) = break (== startChar) xs

expQ :: Parsed -> ExpQ
expQ (Literal s) = litE $ stringL s
expQ (Expression s) =
  case parseExp s of
    Left str -> fail str
    Right x -> return x

toExp :: [Parsed] -> ExpQ
toExp = fmap (foldr1 f) . mapM expQ where
  f a b = InfixE (Just a) (VarE $ mkName "<>") (Just b)

interpolate :: (String -> [Parsed]) -> QuasiQuoter
interpolate f = QuasiQuoter
  { quoteExp  = toExp . f
  , quotePat  = notSupported
  , quoteType = notSupported
  , quoteDec  = notSupported
  }

q :: QuasiQuoter
q = interpolate $ uncurry parse . detectConfig
