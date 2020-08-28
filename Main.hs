{-# LANGUAGE FlexibleContexts #-}
module Main where

import Data.Foldable hiding (find)
import Data.Array.Unboxed
import Data.Array.MArray
import Data.Array.ST
import Control.Monad.ST
import Control.Monad.Trans.Maybe
import Control.Monad.Trans hiding (lift)
import Control.Monad
import Control.Monad.Trans.Writer.CPS
import Control.Monad.Trans.Class
import Data.Monoid


main :: IO ()
main = do
        --print $ primes 100
        print $ length (primes $ (1000*1000*1000))
        return ()


primes max = runST $ do
                       arr <- createSieve max
                       forM_ [2 .. max] $ readAndMark arr
                       (_, Dual ls) <- runWriterT $ forM_ [max, max-1 .. 2] (collector arr)
                       --ls <- collect arr
                       return ls

collector :: STUArray s Int Bool -> Int -> WriterT (Dual [Int]) (ST s) ()
collector arr ind = do
                      b <- lift $ readArray arr ind
                      when b $ tell (Dual [ind])

collect :: STUArray s Int Bool -> ST s [Int]
collect arr = let
                extractor = map fst . filter snd
              in
                do
                  ls <- getAssocs arr
                  return $ extractor ls
                
                


fill :: STUArray s Int Bool -> Int -> Int -> ST s ()
fill arr start step = do 
                        bnd <- snd <$> getBounds arr 
                        forM_ [start, start+step .. bnd] $ flip (writeArray arr) False


readAndMark :: STUArray s Int Bool -> Int -> ST s ()
readAndMark arr i = do
                      isPrime <- readArray arr i
                      when isPrime $ fill arr (i*i) i
                      

createSieve :: Int -> ST s (STUArray s Int Bool)
createSieve max = newArray (2, max) True

