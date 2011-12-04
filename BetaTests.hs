import Char
import Control.Monad
import Data.Attoparsec.ByteString
import Data.Bits
import qualified Data.ByteString as BS
import Data.Maybe
import qualified Data.Word as W
import List
import Test.QuickCheck
import Text.Printf

import Baskerville.Beta.Packets

-- From some tutorial somewhere.
mapper (d, t) = printf "%-30s: " d >> t
main = mapM_ mapper tests

instance Arbitrary BS.ByteString where
    arbitrary = liftM BS.pack (arbitrary :: Gen [W.Word8])
    shrink = map BS.pack . (shrink :: [W.Word8] -> [[W.Word8]]) . BS.unpack

-- | Pull a parser into Maybe.
maybeParse :: Parser a -> BS.ByteString -> Maybe a
maybeParse p bs = maybeResult $ parse p bs

-- | Pull a parser into Maybe and use Maybe to determine whether the parse
--   succeeded.
parseBuild :: Parser a -> (a -> BS.ByteString) -> BS.ByteString -> Bool
parseBuild parser builder bs = case
    maybeParse parser bs >>= Just . builder of
        Just result -> bs == result
        Nothing -> True

buildParse :: Eq a => (a -> BS.ByteString) -> Parser a -> a -> Bool
buildParse builder parser x = case
    maybeParse parser $ builder x of
        Just result -> x == result
        Nothing -> False

-- Baskerville.Beta.Packets
parseBuildWord16 = parseBuild pWord16 bWord16
buildParseWord16 = buildParse bWord16 pWord16
parseBuildWord32 = parseBuild pWord32 bWord32
buildParseWord32 = buildParse bWord32 pWord32

tests = [("bWord16.pWord16/id", quickCheck buildParseWord16)
        ,("bWord32.pWord32/id", quickCheck buildParseWord32)]
