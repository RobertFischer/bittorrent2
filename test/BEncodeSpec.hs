{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module BEncodeSpec (spec) where

import qualified Data.ByteString                               as SBS
import qualified Data.ByteString.Builder                       as BSB
import qualified Data.ByteString.Char8                         as C8
import qualified Data.ByteString.Lazy                          as LBS
import qualified Data.List                                     as List
import           Data.Text                                     ( Text )
import           Data.Text                                     as T
import           Generic.Random
import           GHC.Generics
import           Numeric                                       ( showHex, showInt )
import           RFC.Prelude
import           Test.Hspec
import           Test.Hspec.Core.QuickCheck                    as QC
import qualified Test.Hspec.SmallCheck                         as SC
import           Test.Invariant
import           Test.QuickCheck                               as QC
import           Test.QuickCheck.Classes
import           Test.QuickCheck.Instances.ByteString
import           Test.QuickCheck.Instances.Containers
import           Test.QuickCheck.Instances.Natural
import           Test.QuickCheck.Instances.Text
import           Test.QuickCheck.Instances.UnorderedContainers
import           Test.QuickCheck.Modifiers
import qualified Test.QuickCheck.Unicode                       as QCU
import qualified Test.SmallCheck                               as SC
import           Test.SmallCheck.Series                        ( (\/) )
import qualified Test.SmallCheck.Series                        as SC

import           BEncode

instance (Monad m) => SC.Serial m Word64 where
	series = (fromIntegral::Integer -> Word64) <$> SC.series

instance (Monad m) => SC.Serial m Word8 where
	series = (fromIntegral::Integer -> Word8) <$> SC.series

instance (Monad m) => SC.Serial m ByteString where
	series =
		SC.cons1 (encodeUtf8 . T.pack) \/
		SC.cons1 (C8.singleton) \/
		SC.cons1 (LBS.toStrict . BSB.toLazyByteString . BSB.integerDec) \/
		SC.cons1 (LBS.toStrict . BSB.toLazyByteString . BSB.word64Hex) \/
		SC.cons1 (\w1 -> SBS.singleton w1)

instance (Monad m) => SC.Serial m Text where
	series = T.pack <$> SC.series

newtype HexString = HexString String deriving (Eq,Ord,Show,Generic,Typeable)
instance Arbitrary HexString where
	arbitrary = sized $ \n -> do
		chosenInt <- choose (0,n)
		return . HexString $ showHex chosenInt ""
instance (Monad m) => SC.Serial m HexString where
	series = (HexString . (flip showHex "") . abs) <$> SC.series


newtype DecString = DecString String deriving (Eq,Ord,Show,Generic,Typeable)
instance Arbitrary DecString where
	arbitrary = sized $ \n -> do
		chosenInt <- choose (0,n)
		return . DecString $ showInt chosenInt ""
instance (Monad m) => SC.Serial m DecString where
	series = (DecString . (flip showInt "") . abs) <$> SC.series

newtype ZeroString = ZeroString String deriving (Eq,Ord,Show,Generic,Typeable)
instance Arbitrary ZeroString where
	arbitrary = sized $ \n -> do
		chosenSize <- choose (1, n+1)
		return . ZeroString $ List.replicate chosenSize '0'
instance (Monad m) => SC.Serial m ZeroString where
	series = (ZeroString . (flip List.replicate '0') . abs) <$> SC.series

newtype IntegerBiggerThanWord = IntegerBiggerThanWord Integer deriving (Eq,Ord,Show,Generic,Typeable)
instance Arbitrary IntegerBiggerThanWord where
	arbitrary = sized $ \n -> do
		chosen <- choose(toInteger (maxBound::Word), toInteger (maxBound::Word) + toInteger n + 1)
		return . IntegerBiggerThanWord $ chosen
instance (Monad m) => SC.Serial m IntegerBiggerThanWord where
	series =
		SC.cons1 (\n -> IntegerBiggerThanWord . toInteger $ n * (maxBound::Word)) \/
		SC.cons1 (\n -> IntegerBiggerThanWord . toInteger $ n * (maxBound::Word) * (-1))


spec :: Spec
spec = do
	describe "testing framework" $ do
		it "works" $ do
			True `shouldBe` True

	describe "make -> get roundtrip" $ do

		describe "on BBytes" $ do
			let theTest = \x -> (getBBytes . mkBBytes) x == x
			it "is equal for basic values" $ property $
				theTest
			it "is equal for small values" $ SC.property $ theTest
			it "is equal for big values" $ property $
				mapSize (*256) $ theTest

		describe "on BBytesL" $ do
			let theTest = \x -> (getBBytesL . mkBBytesL) x == x
			it "is equal for basic values" $ property $
				theTest
			it "is equal for small values" $ property $
				mapSize (\n -> if n < 1024*8 then n else 1024*8) $ theTest
			it "is equal for big values" $ property $
				mapSize (*256) $ theTest

		describe "on BChar" $ do
			let theTest = \c -> (getBBytes . mkBChar) c == (encodeUtf8 . toText) [c]
			it "is equal for basic values" $ property $ theTest
			it "is equal for small values" $ SC.property $ theTest

		describe "on BString" $ do
			let theTest = \str -> (getBBytes . mkBString) str == (encodeUtf8 . toText) str
			it "is equal for basic values" $ property $ theTest
			it "is equal for small values" $ SC.property $ theTest
			it "is equal for big values" $ property $ mapSize (*256) $ theTest

		describe "on hex BStrings" $ do
			let theTest = \(HexString str) -> (getBBytes . mkBString) str == (encodeUtf8 . toText) str
			it "is equal for basic values" $ property $ theTest
			it "is equal for small values" $ SC.property $ theTest
			it "is equal for big values" $ property $ mapSize (*256) $ theTest

		describe "on decimal BStrings" $ do
			let theTest = \(DecString str) -> (getBBytes . mkBString) str == (encodeUtf8 . toText) str
			it "is equal for basic values" $ property $ theTest
			it "is equal for small values" $ SC.property $ theTest
			it "is equal for big values" $ property $ mapSize (*256) $ theTest

		describe "on zero BStrings" $ do
			let theTest = \(ZeroString str) -> (getBBytes . mkBString) str == (encodeUtf8 . toText) str
			it "is equal for basic values" $ property $
				theTest
			it "is equal for small values" $ property $
				mapSize (\n -> if n < 1024*8 then n else 1024*8) $ theTest
			it "is equal for big values" $ property $
				mapSize (*256) $ theTest

		describe "on hex BStrings with leading zeroes" $ do
			let theTest = \(ZeroString start, HexString end) ->
				(getBBytes . mkBString) (start <> end) == (encodeUtf8 . toText) (start <> end)
			it "is equal for basic values" $ property $
				theTest
			it "is equal for small values" $ property $
				mapSize (\n -> if n < 1024*8 then n else 1024*8) $ theTest
			it "is equal for big values" $ property $ mapSize (*256) $ theTest

		describe "on decimal BStrings with leading zeroes" $ do
			let theTest = \(ZeroString start, DecString end) ->
				(getBBytes . mkBString) (start <> end) == (encodeUtf8 . toText) (start <> end)

			it "is equal for basic values" $ property $ theTest
			it "is equal for small values" $ SC.property $ theTest
			it "is equal for big values" $ property $ mapSize (*256) $ theTest

		describe "on BText" $ do
			let theTest = \(txt::StrictText) -> (getBBytes . mkBText) txt == (encodeUtf8 . toText) txt
			it "is equal for basic values" $ property $ theTest
			it "is equal for small values" $ SC.property $ theTest
			it "is equal for big values" $ property $ mapSize (*256) $ theTest

		describe "on BIntegral" $ do
			let theTest = \(z::Word) -> (getBInteger . mkBIntegral) z == toInteger z
			it "is equal for basic values" $ property $ theTest
			it "is equal for small values" $ SC.property $ theTest

		describe "on BInteger" $ do
			let theTest = \z -> (getBInteger . mkBInteger) z == z
			it "is equal for basic values" $ property $ theTest
			it "is equal for small values" $ SC.property $ theTest

		describe "on BInteger with big positive values" $ do
			let theTest = \(IntegerBiggerThanWord z) -> (getBInteger . mkBInteger) z == z
			it "is equal for basic values" $ property $ theTest
			it "is equal for small values" $ SC.property $ theTest

		describe "on BInteger with big negative values" $ do
			let theTest = \(IntegerBiggerThanWord z) -> (getBInteger . mkBInteger) (-1 * z) == (-1 * z)
			it "is equal for basic values" $ property $ theTest
			it "is equal for small values" $ SC.property $ theTest
