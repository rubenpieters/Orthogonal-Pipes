module Main (main) where

import           Data.Conduit as C
import qualified Data.Conduit.Combinators as C

import           Pipes as P
import qualified Pipes.Prelude as P

import qualified Streaming.Prelude as Str
import           Streaming.Internal
import           Data.Functor.Of

import qualified System.IO.Streams as IOS

import qualified Data.Vector.Fusion.Stream.Monadic as V

import qualified Data.Machine as M
import           Data.Machine.Runner  (foldlT)

import qualified OrthPipes.Plan as O
import qualified OrthPipes.Prelude as O
import           OrthPipes.Proxy

import           Control.Exception
import           Criterion.Main

import           Data.Functor.Identity (runIdentity)
import           Data.Function ((&))

import qualified Data.List as L

-------------------------------------------------
-- main
-------------------------------------------------

main :: IO ()
main = do
  print ("basicResult " ++ show basicResult)
  print ("sumResult " ++ show sumResult)
  print ("primesResult " ++ show primesResult)
  defaultMain
      [ bgroup "length-io"   [ bench "streaming"      $ nfAppIO streaming_basic basicN
                             , bench "conduit"        $ nfAppIO conduit_basic basicN
                             , bench "pipes"          $ nfAppIO pipes_basic basicN
                             , bench "iostreams"      $ nfAppIO iostreams_basic basicN
                             , bench "machine"        $ nfAppIO machines_basic basicN
                             , bench "vector"         $ nfAppIO vector_basic basicN
                             , bench "orth-pipes"     $ nfAppIO orthpipes_basic basicN
                             ]
      , bgroup "length-id"   [ bench "streaming"      $ nf (runIdentity . streaming_basic) basicN
                             , bench "conduit"        $ nf (runIdentity . conduit_basic) basicN
                             , bench "pipes"          $ nf (runIdentity . pipes_basic) basicN
                             , bench "machine"        $ nf (runIdentity . machines_basic) basicN
                             , bench "vector"         $ nf (runIdentity . vector_basic) basicN
                             , bench "orth-pipes"     $ nf (runIdentity . orthpipes_basic) basicN
                             ]
      , bgroup "sum-io"      [ bench "streaming"      $ nfAppIO streaming_sum sumN
                             , bench "conduit"        $ nfAppIO conduit_sum sumN
                             , bench "pipes"          $ nfAppIO pipes_sum sumN
                             , bench "iostreams"      $ nfAppIO iostreams_sum sumN
                             , bench "machine"        $ nfAppIO machines_sum sumN
                             , bench "vector"         $ nfAppIO vector_sum sumN
                             , bench "orth-pipes"     $ nfAppIO orthpipes_sum sumN
                             ]
      , bgroup "sum-id"      [ bench "streaming"      $ nf (runIdentity . streaming_sum) sumN
                             , bench "conduit"        $ nf (runIdentity . conduit_sum) sumN
                             , bench "pipes"          $ nf (runIdentity . pipes_sum) sumN
                             , bench "machine"        $ nf (runIdentity . machines_sum) sumN
                             , bench "vector"         $ nf (runIdentity . vector_sum) sumN
                             , bench "orth-pipes"     $ nf (runIdentity . orthpipes_sum) sumN
                             ]
      , bgroup "primes-io"   [ bench "conduit"        $ nfAppIO conduit_primes primesN
                             , bench "pipes"          $ nfAppIO pipes_primes primesN
                             , bench "orth-pipes"     $ nfAppIO orthpipes_primes primesN
                             ]
      , bgroup "primes-id"   [ bench "conduit"        $ nf (runIdentity . conduit_primes) primesN
                             , bench "pipes"          $ nf (runIdentity . pipes_primes) primesN
                             , bench "orth-pipes"     $ nf (runIdentity . orthpipes_primes) primesN
                             ]
      ]

-------------------------------------------------
-- length benchmark
-------------------------------------------------

basicN :: Int
basicN = 10000

basicResult :: Int
basicResult = Prelude.length $
    [1..basicN]
  & Prelude.filter even
  & Prelude.map (+1)
  & Prelude.drop 1000
  & Prelude.map (+1)
  & Prelude.filter (\x -> x `mod` 2 == 0)

streaming_basic :: (Monad m) => Int -> m Int
streaming_basic to = do
  xs <- Str.toList_ $
            Str.each [1..to]
          & Str.filter even
          & Str.map (+1)
          & Str.drop 1000
          & Str.map (+1)
          & Str.filter (\x -> x `mod` 2 == 0)
  return $ checkBasic (Prelude.length xs)

conduit_basic :: (Monad m) => Int -> m Int
conduit_basic to = do
  xs <- runConduit $
             C.yieldMany [1..to]
          .| C.filter even
          .| C.map ((+1) :: Int -> Int)
          .| (C.drop 1000
          >> C.map ((+1) :: Int -> Int)
          .| C.filter (\x -> x `mod` 2 == 0)
          .| C.sinkList
          )
  return $ checkBasic (Prelude.length xs)

pipes_basic :: (Monad m) => Int -> m Int
pipes_basic to = do
  xs <- P.toListM $
              P.each [1..to]
          >-> P.filter even
          >-> P.map (+1)
          >-> P.drop 1000
          >-> P.map (+1)
          >-> P.filter (\x -> x `mod` 2 == 0)
  return $ checkBasic (Prelude.length xs)

iostreams_basic :: Int -> IO Int
iostreams_basic to = do
  s0 <- IOS.fromList [1..to]
  s1 <- IOS.filter even s0
  s2 <- IOS.map (+1) s1
  s3 <- IOS.drop 1000 s2
  s4 <- IOS.map (+1) s3
  s5 <- IOS.filter (\x -> x `mod` 2 == 0) s4
  xs <- IOS.toList s5
  assert (Prelude.length xs == basicResult) $
      return (Prelude.length (xs :: [Int]))

machines_basic :: (Monad m) => Int -> m Int
machines_basic to = do
  xs <- M.runT $
               M.source [1..to :: Int]
          M.~> M.filtered even
          M.~> M.mapping (+1)
          M.~> M.dropping 1000
          M.~> M.mapping (+1)
          M.~> M.filtered (\x -> x `mod` 2 == 0)
  return $ checkBasic (Prelude.length xs)

vector_basic :: (Monad m) => Int -> m Int
vector_basic to = do
  xs <- V.toList
           $ V.filter even
           $ V.map (+1)
           $ V.drop 1000
           $ V.map (+1)
           $ V.filter (\x -> x `mod` 2 == 0)
           $ V.enumFromTo 1 to
  return $ checkBasic (Prelude.length xs)

orthpipes_basic :: (Monad m) => Int -> m Int
orthpipes_basic to = do
  xs <- O.fetchResponses $
                O.each [1..to]
          O.>-> O.filter even
          O.>-> O.map (+1)
          O.>-> O.drop 1000
          O.>-> O.map (+1)
          O.>-> O.filter (\x -> x `mod` 2 == 0)
  return $ checkBasic (Prelude.length xs)

-------------------------------------------------
-- sum benchmark
-------------------------------------------------

sumN :: Int
sumN = 1000000

sumResult :: Int
sumResult = L.foldl' (+) 0 $
    [1..sumN]
  & Prelude.filter even
  & Prelude.map (+1)
  & Prelude.drop 1000
  & Prelude.map (+1)
  & Prelude.filter (\x -> x `mod` 2 == 0)

streaming_sum :: (Monad m) => Int -> m Int
streaming_sum to = do
  n <- Str.sum_ $
           Str.each [1..to]
         & Str.filter even
         & Str.map (+1)
         & Str.drop 1000
         & Str.map (+1)
         & Str.filter (\x -> x `mod` 2 == 0)
  return $ checkSum n

conduit_sum :: (Monad m) => Int -> m Int
conduit_sum to = do
  n <- runConduit $
               C.yieldMany [1..to]
            .| C.filter even
            .| C.map ((+1) :: Int -> Int)
            .| (C.drop 1000
            >> C.map ((+1) :: Int -> Int)
            .| C.filter (\x -> x `mod` 2 == 0)
            .| C.foldl (+) 0
            )
  return $ checkSum n

pipes_sum :: (Monad m) => Int -> m Int
pipes_sum to = do
  n <- P.fold (+) 0 id $
             P.each [1..to]
         >-> P.filter even
         >-> P.map (+1)
         >-> P.drop 1000
         >-> P.map (+1)
         >-> P.filter (\x -> x `mod` 2 == 0)
  return $ checkSum n

iostreams_sum :: Int -> IO Int
iostreams_sum to = do
  s0 <- IOS.fromList [1..to]
  s1 <- IOS.filter even s0
  s2 <- IOS.map (+1) s1
  s3 <- IOS.drop 1000 s2
  s4 <- IOS.map (+1) s3
  s5 <- IOS.filter (\x -> x `mod` 2 == 0) s4
  n <- IOS.fold (+) 0 s5
  assert (n == sumResult) $
      return n

machines_sum :: (Monad m) => Int -> m Int
machines_sum to = do
  n <- foldlT (+) 0 $
              M.source [1..to :: Int]
         M.~> M.filtered even
         M.~> M.mapping (+1)
         M.~> M.dropping 1000
         M.~> M.mapping (+1)
         M.~> M.filtered (\x -> x `mod` 2 == 0)
  return $ checkSum n

vector_sum :: (Monad m) => Int -> m Int
vector_sum to = do
  n <- V.foldl (+) 0
          $ V.filter even
          $ V.map (+1)
          $ V.drop 1000
          $ V.map (+1)
          $ V.filter (\x -> x `mod` 2 == 0)
          $ V.enumFromTo 1 to
  return $ checkSum n

orthpipes_sum :: (Monad m) => Int -> m Int
orthpipes_sum to = do
  n <- O.foldResponses (+) 0 $
                O.each [1..to]
          O.>-> O.filter even
          O.>-> O.map (+1)
          O.>-> O.drop 1000
          O.>-> O.map (+1)
          O.>-> O.filter (\x -> x `mod` 2 == 0)
  return $ checkSum n

-------------------------------------------------
-- primes benchmark
-------------------------------------------------
-- only conduit/pipes/orth-pipes since this
-- benchmark is meant to bench nested merging

primesN :: Int
primesN = 10000

list_sieve :: [Int] -> [Int]
list_sieve (p:xs) = p : list_sieve (Prelude.filter (\x -> x `mod` p /= 0) xs)
list_sieve [] = []

primesResult :: Int
primesResult = Prelude.last $ Prelude.take primesN $ list_sieve [2..]

conduit_sieve :: (Monad m) => ConduitT Int Int m ()
conduit_sieve = do
  mP <- C.await
  case mP of
    Just p -> do
      C.yield p
      C.filter (\x -> x `mod` p /= 0) .| conduit_sieve
    Nothing -> return ()

conduit_primes :: (Monad m) => Int -> m Int
conduit_primes n = do
  xs <- runConduit $
             C.yieldMany [2..]
          .| conduit_sieve
          .| C.take n
          .| C.sinkList
  return $ checkPrimes (Prelude.last xs)

pipes_sieve :: (Monad m) => Pipe Int Int m x
pipes_sieve = do
  p <- P.await
  P.yield p
  P.filter (\x -> x `mod` p /= 0) >-> pipes_sieve

pipes_primes :: (Monad m) => Int -> m Int
pipes_primes n = do
  xs <- P.toListM $
              P.each [2..]
          >-> pipes_sieve
          >-> P.take n
  return $ checkPrimes (Prelude.last xs)

orthpipes_sieve :: (Monad m) => O.Plan () Int () Int m r
orthpipes_sieve = do
  p <- O.await
  O.yield p
  O.filter (\x -> x `mod` p /= 0) O.>-> orthpipes_sieve

orthpipes_primes :: (Monad m) => Int -> m Int
orthpipes_primes n = do
  ps <- O.fetchResponses $
                O.upfrom 2
          O.>-> orthpipes_sieve
          O.>-> O.take n
  return $ checkPrimes (Prelude.last ps)

-------------------------------------------------
-- check helpers
-------------------------------------------------

checkBasic :: Int -> Int
checkBasic x = check x basicResult

checkSum :: Int -> Int
checkSum x = check x sumResult

checkPrimes :: Int -> Int
checkPrimes x = check x primesResult

check :: (Eq a, Show a) => a -> a -> a
check received expected =
  if received /= expected
    then error ("incorrect result: expected " ++ show expected ++ " got " ++ show received)
    else received
