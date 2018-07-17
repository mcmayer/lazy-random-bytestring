module Main where

import           Control.Monad                (when)
import qualified Data.ByteString.Lazy         as BSL
import           Data.ByteString.Lazy.Builder (Builder, toLazyByteString, word8)
import           Data.Word8                   (Word8 (..))
import           System.Environment           (getArgs)
import           System.Exit                  (exitFailure)
import           System.IO                    (hPutStrLn, stderr, stdout)
import           System.Random

-- |Version 1: Recurse with cons
lazyRandomByteString :: Int -> StdGen -> BSL.ByteString
lazyRandomByteString n g = fst3 $ iter (BSL.empty, n, g) where
    fst3 (a, _, _) = a
    iter (bs', n', g') =
        if n' == 0 then (bs', 0, g')
        else iter (w `BSL.cons` bs', n'-1, g'') where
            (w, g'') = random g' :: (Word8, StdGen)

-- |Version 2: Recurse with unfoldr
lazyRandomByteString' :: Int -> StdGen -> BSL.ByteString
lazyRandomByteString' n g = BSL.unfoldr f (n, g) where
    f (n', g') =
        if n' == 0 then Nothing
        else Just (w, (n'-1, g'')) where
            (w, g'') = random g' :: (Word8, StdGen)

-- |Version 3: Use `Builder`
lazyRandomByteString'' :: Int -> StdGen -> BSL.ByteString
lazyRandomByteString'' n g = toLazyByteString $ builder where
    builder :: Builder
    builder = fst3 $ iter (mempty, n, g) where
        fst3 (a, _, _) = a
        iter :: (Builder, Int, StdGen) -> (Builder, Int, StdGen)
        iter (b, n', g') =
            if n' == 0 then (b, 0, g')
            else iter (b <> (word8 w), n'-1, g'') where
                (w, g'') = random g' :: (Word8, StdGen)

main :: IO ()
main = do
    args <- getArgs
    when (null args) $ hPutStrLn stderr "How many bytes do you want?" >> exitFailure
    let len = read (head args) :: Int
    gen <- getStdGen
    BSL.hPutStr stdout (lazyRandomByteString'' len gen)
