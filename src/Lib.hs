{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications      #-}
module Lib where

import           Debug.Trace                (trace)

import           Control.Monad.ST
import           Data.List
import           Data.Monoid
import           Prelude                    as P

import           Control.Monad.State.Strict as S
import           Data.Massiv.Array          as A

alphabet = "?abcdefghijklmnopqrstuvwxyz "
n = length alphabet

type Txt = Array U Ix1 Int

{- Single Letter Statistics -}

mostCommonLetters = "e taonisrhldcumfpwgbyvkxjqz?"

singleLetterStats :: Txt -> Array U Ix1 Int
singleLetterStats txt = runST $
  do
    counter <- newMArray' @U @Ix1 @Int (Sz1 $ length alphabet)
    traverseA_ (\l -> modify_ counter (\c -> return $ c+1) l) txt

    freezeS counter

initialPermutation :: Permutation U
initialPermutation = makeArray Seq (Sz $ length alphabet) f
  where f :: Int -> Int
        f idx = case find (== alphabet !! idx) mostCommonLetters of
                  Just c -> toIndice c
                  Nothing -> error "Bug"

{- Linear Congruential Generator -}

type Rand = State Int

congruential :: Int -> Int
congruential x = (x * 21938912307 + 9873427) `mod`  81263789677

generateRandom :: Int -> Rand Int
generateRandom bound = do
  S.modify congruential
  (`mod` bound) <$> get

{- Conversions -}

toIndice :: Char -> Int
toIndice c =
  case elemIndex c alphabet of
    Just i -> i
    _      -> 0

string2repr :: String -> Txt
string2repr txt =
  let arr = A.fromList Seq txt :: Array U Ix1 Char  in
  compute $ A.map toIndice arr

{- Permutation -}

type Permutation r = Array r Ix1 Int

identity :: Permutation D
identity = makeArray Seq (Sz n) id

mutate :: Permutation D -> Rand (Permutation D)
mutate p = do
    r1 <- generateRandom n
    r2 <- generateRandom (n-1)

    let i = r1
        j = (r1 + 1 + r2) `mod` n
    let f idx | idx == i = j
              | idx == j = i
              | otherwise = idx

    return $ A.backpermute' (Sz n) f p

applyPermutation :: Permutation P -> Txt -> Txt
applyPermutation p txt = compute $ A.map (A.index' p) txt

generatePermutation :: Rand (Permutation D)
generatePermutation = do
    foldM (\p k ->
        do
          r1 <- generateRandom k
          r2 <- generateRandom (k-1)

          let i = r1
              j = (r1 + 1 + r2) `mod` k
          let f idx | idx == i = j
                    | idx == j = i
                    | otherwise = idx

          return $ A.backpermute' (Sz n) f p

       ) identity [2..n]


{- Optimization -}

costFunction :: Statistics -> Txt -> Float
costFunction stats txt =

    let arr' = compute arr :: Array P Ix1 Float in
    log $ getProduct $ foldMono Product arr'

  where arr = applyStencil noPadding (stencil stats) txt

stencil :: Statistics -> Stencil Ix1 Int Float
stencil stats = makeStencil (Sz 2) 0 f
  where f getter = stats ! (getter 0) :. (getter 1)

selectBest :: Int -> Statistics -> Txt -> [Permutation P] -> [Permutation P]
selectBest n stats txt l =
    let l' = P.map f l in
    P.map fst $ P.take n $ sortBy order l'
  where f p =
          let txt' = applyPermutation p txt
              cost = costFunction stats txt' in
          (p,cost)
        order (_,costA) (_,costB) = compare costA costB

{- Statistics -}

type Statistics = Array U Ix2 Float

loadStatistics :: FilePath -> IO Statistics
loadStatistics path = do
    file <- readFile path

    let parsed = P.map parser (lines file)

    let sorted = sortBy order $ P.map (\(a,b,p) -> (toIndice a, toIndice b,p)) parsed

    return $ makeArray Par (Sz $ n :. n)
                (\(i :. j) -> let (_,_,r) = sorted !! (i*n + j) in r )

  where parser (a:b:s) = (a,b, P.read s :: Float)
        parser l       = error $ "Invalid format" ++  show l

        order (a,b,_) (c,d,_) =
          case compare a c of
            EQ  -> compare b d
            ord -> ord

computeStatistics :: FilePath -> FilePath -> IO ()
computeStatistics pathCorpus pathStats = do
    corpus <- readFile pathCorpus

    let sigma = length corpus - 1

    let txt = string2repr corpus
    let stats = count txt
    let contents =
          do
            i <- [0..n-1]
            j <- [0..n-1]

            let a = alphabet !! i
                b = alphabet !! j

            return $ (a, b, (fromIntegral $ stats ! (i :. j)) / fromIntegral sigma)

    writeFile pathStats $ unlines (P.map serialize contents)

  where serialize (a,b,proba) = [a,b] ++ show proba


count :: Txt -> Array U Ix2 Int
count txt = runST $
  do
    let Sz1 n = A.size txt
    acc <- newMArray' @U @Ix2 @Int (Sz $ n :. n)

    let incr i j = modifyM_ acc (\e -> return (e+1)) (Ix2 i j)
    let pairs = applyStencil @U noPadding pairify txt

    A.forM_ (compute @U pairs) (\(i,j) -> incr i j)

    freezeS acc

  where pairify :: Stencil Ix1 Int (Int,Int)
        pairify = makeStencil (Sz 2) 0 $ \getter -> (getter 0, getter 1)

