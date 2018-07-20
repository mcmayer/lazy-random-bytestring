{-# LANGUAGE BangPatterns #-}
module Main where

import           Control.Monad                (when)
import           Data.Bits                    (shiftL, shiftR, xor, (.&.))
import qualified Data.ByteString.Lazy         as BSL
import           Data.ByteString.Lazy.Builder (Builder, toLazyByteString, word8)
import           Data.Word8                   (Word8 (..))
import           System.Environment           (getArgs)
import           System.Exit                  (exitFailure)
import           System.IO                    (hPutStrLn, stderr, stdout)
import           System.Random

{- XorRNG is a simple old XOR-based RNG -}
newtype XorRNG = XorRNG Word

newXorRNG :: IO XorRNG
newXorRNG = XorRNG <$> (randomIO :: IO Word)

{-# INLINE rand #-}
rand :: XorRNG -> (Word8, XorRNG)
rand (XorRNG !x) = (fromIntegral x3, XorRNG x3) where
    x1 = x `xor` (x `shiftL` 13)
    x2 = x1 `xor` (x1 `shiftR` 17)
    x3 = x2 `xor` (x2 `shiftL` 5)


data Iter1 = Iter1 BSL.ByteString !Int !XorRNG

-- |Version 1: Recurse with cons
lazyRandomByteString1 :: Int -> XorRNG -> BSL.ByteString
lazyRandomByteString1 n g = fst3 $ iter (Iter1 BSL.empty n g) where
    fst3 (Iter1 a _ _) = a
    {-# INLINE [0] iter #-}
    iter (Iter1 bs' n' g') =
        if n' == 0 then Iter1 bs' 0 g'
        else iter (Iter1 (w `BSL.cons` bs') (n'-1) g'') where
            (w, g'') = rand g'

data Iter2 = Iter2 !Int !XorRNG

-- |Version 2: Recurse with unfoldr
lazyRandomByteString2 :: Int -> XorRNG -> BSL.ByteString
lazyRandomByteString2 n g = BSL.unfoldr f (Iter2 n g) where
    {-# INLINE f #-}
    f (Iter2 n' g') =
        if n' == 0 then Nothing
        else Just (w, Iter2 (n'-1) g'') where
            (w, g'') = rand g'

data Iter2' = Iter2' !Int !StdGen

lazyRandomByteString2' :: Int -> StdGen -> BSL.ByteString
lazyRandomByteString2' n g = BSL.unfoldr f (Iter2' n g) where
    {-# INLINE f #-}
    f (Iter2' n' g') =
        if n' == 0 then Nothing
        else Just (w, Iter2' (n'-1) g'') where
            (w, g'') = random g' :: (Word8, StdGen)

data Iter3 = Iter3 Builder !Int !XorRNG

-- |Version 3: Use [`Builder`](https://hackage.haskell.org/package/bytestring-0.10.0.2/docs/Data-ByteString-Lazy-Builder.html#t:Builder)
lazyRandomByteString3 :: Int -> XorRNG -> BSL.ByteString
lazyRandomByteString3 n g = toLazyByteString builder where
    builder :: Builder
    builder = fst3 $ iter (Iter3 mempty n g) where
        fst3 (Iter3 a _ _) = a
        {-# INLINE iter #-}
        iter :: Iter3 -> Iter3
        iter (Iter3 b n' g') =
            if n' == 0 then (Iter3 b 0 g')
            else iter (Iter3 (b <> (word8 w)) (n'-1) g'') where
                (w, g'') = rand g'

main :: IO ()
main = do
    args <- getArgs
    when (null args) $ hPutStrLn stderr "How many bytes do you want?" >> exitFailure
    let len = read (head args) :: Int
    gen <- newXorRNG
    BSL.hPut stdout (lazyRandomByteString3 len gen)
    --gen <- getStdGen
    --BSL.hPut stdout (lazyRandomByteString2' len gen)
