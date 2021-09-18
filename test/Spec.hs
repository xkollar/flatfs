{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main (main) where

import GHC.Num ((-))
import GHC.Real (div, fromIntegral)

import Control.Applicative ((<*>), pure)
import Data.Binary (decode, encode)
import Data.Bool (Bool)
import Data.Coerce (coerce)
import Data.Eq (Eq, (==), (/=))
import Data.Function ((.), id)
import Data.Functor ((<$>), fmap)
import Data.Maybe (maybe)
import Data.Ord (Ord, (>))
import Data.String (fromString)
import Data.Tuple (uncurry)
import System.IO (IO)

import Data.ByteString.Lazy (ByteString, length)
import Data.ByteString.Lazy.Char8 (unpack)
import qualified Data.Set as Set (filter)
import Data.UUID (UUID, fromWords)
import Test.Framework (defaultMain, testGroup, Test)
-- import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
-- import Test.HUnit hiding (Test)
import Test.QuickCheck (Arbitrary, arbitrary, arbitraryBoundedEnum, resize, shrink, allProperties)
import Test.QuickCheck.Arbitrary (genericShrink)

import Lib
    ( BlockSize
    , Delimiters(Delimiters)
    , Header(Header)
    , Label(Label)
    , NextExtentMapBlock(NextExtentMapBlock)
    , Superblock(Superblock)
    , Version(Version)
    , fromBlockSize
    , superblockSize
    , toNextExtentMapBlock
    )

instance Arbitrary Version where
    arbitrary = Version <$> arbitrary <*> arbitrary
    shrink = genericShrink

instance Arbitrary Label where
    arbitrary = fromString <$> resize 16 arbitrary
    shrink = fmap fromString . shrink . unpack . coerce @Label @ByteString

instance Arbitrary UUID where
    arbitrary = fromWords <$> gen <*> gen <*> gen <*> gen
      where
        gen = resize 64 arbitrary
    shrink _ = []

instance Arbitrary BlockSize where
    arbitrary = arbitraryBoundedEnum
    shrink = genericShrink

instance Arbitrary Delimiters where
    arbitrary = Delimiters . Set.filter (0 /=) <$> arbitrary
    shrink _ = []

instance Arbitrary NextExtentMapBlock where
    arbitrary = toNextExtentMapBlock <$> arbitrary
    shrink (NextExtentMapBlock n) = maybe [] f n
      where
        f x = toNextExtentMapBlock <$> [x `div` 2, x-1]

minMax :: Ord a => a -> a -> (a, a)
minMax x y = if x > y then (y, x) else (x, y)

instance Arbitrary Header where
    arbitrary = do
        (bs, sbs) <- minMax <$> arbitrary <*> arbitrary
        Header <$> arbitrary <*> pure sbs <*> pure bs <*> arbitrary <*> arbitrary <*> arbitrary
    shrink _ = []

instance Arbitrary Superblock where
    arbitrary = do
        h@Header{superblockSize} <- arbitrary
        next <- arbitrary
        let s = length (encode h) - length (encode next)
            n = (fromBlockSize superblockSize - s) `div` 64
        Superblock h <$> resize (fromIntegral n) arbitrary <*> pure next
    shrink = genericShrink

(=:=) :: Eq a => (b -> a) -> (b -> a) -> b -> Bool
f =:= g = \ x -> f x == g x
infix 4 =:=

prop_decodeEncodeSuperblock :: Superblock -> Bool
prop_decodeEncodeSuperblock = decode . encode =:= id

prop_decodeEncodeLabel :: Label -> Bool
prop_decodeEncodeLabel = decode . encode =:= id

-- TemplateHaskell magic
pure [ ]

tests :: [Test]
tests = [testGroup "Lib" (uncurry testProperty <$> $allProperties)]

main :: IO ()
main = defaultMain tests
