{-# LANGUAGE TypeApplications #-}
module Main where

import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Control.Monad.Trans        hiding (forM)

import qualified Data.List                  as L
import           Data.Semigroup             ((<>))

import           Lib
import           Prelude                    as P

import qualified Data.Massiv.Array          as A
import           Options.Applicative

import           Debug.Trace                (trace)

main :: IO ()
main = do
  config <- execParser $
    info (cliParser <**> helper)
        ( fullDesc
       <> progDesc "Decipher substitution ciphers without the key"
       <> header "decipher - substitution parser attack" )

  if run config then do
    stats <- loadStatistics (statsPath config)
    txt <- string2repr <$> readFile (inputPath config)
    let env = Environment config txt stats

    let runner monad = runState (runReaderT monad env) (seed config)

    let (lastEpoch,_) = runner $
            foldM (\epoch _ -> evolve epoch)
              [A.compute @A.P initialPermutation] [1..lastRun config]

    let [fittest] = selectBest 1 stats txt lastEpoch

    print fittest

    let out = A.toList $ A.map (\idx -> alphabet !! (fittest A.! idx)) txt

    print out
  else do
    computeStatistics (corpusPath config) (statsPath config)
    print "Done."

cliParser = subparser (
    command "run" (info parseRun idm)
    <> command "prepare" (info parsePrepare idm)
  )

parseRun :: Parser Config
parseRun = Config
  <$> option auto (
          long "childrenCount" <> short 'c'
          <> metavar "CHILDREN_COUNT"
          <> value 3
          <> help "How many mutations will be performed on a single permutation")
  <*> option auto (
          long "epochSize" <> short 's'
          <> metavar "EPOCH_SIZE"
          <> value 50
          <> help "How many permutations are kept alive in an epoch")
  <*> pure True
  <*> argument str (metavar "INPUT_PATH" <> help "File path to the input file")
  <*> pure ""
  <*> strOption (
          long "statisticsPath"
          <> metavar "STATS_PATH"
          <> value "statistics.csv"
          <> help "Statistics's CSV file. Line order is relevent during parsing"
        )
  <*> option auto (
          long "lastRun"
          <> metavar "LAST_RUN"
          <> value 1000
          <> help "Number of epochs to do"
        )
  <*> option auto (
          long "seed"
          <> metavar "SEED"
          <> value 42
          <> help "Seed of the random number generator (congruential)"
        )

parsePrepare :: Parser Config
parsePrepare = Config
  <$> pure 0
  <*> pure 0
  <*> pure False
  <*> pure ""
  <*> argument str (metavar "CORPUS_PATH" <> help "File path to the corpus")
  <*> strOption (
          long "statisticsPath"
          <> metavar "STATS_PATH"
          <> value "statistics.csv"
          <> help "Statistics's CSV file. Line order is relevent during parsing"
        )
  <*> pure 0
  <*> pure 0


data Config = Config
    { childrenCount :: Int
    , epochSize     :: Int
    , run           :: Bool
    , inputPath     :: FilePath
    , corpusPath    :: FilePath
    , statsPath     :: FilePath
    , lastRun       :: Int
    , seed          :: Int
    }

data Environment = Environment
    { config     :: Config
    , inputTxt   :: Txt
    , statistics :: Statistics
    }

type Env = ReaderT Environment Rand
type Epoch = [Permutation A.P]

evolve :: Epoch -> Env Epoch
evolve epoch = do

    seed <- get

    epochSize <- asks $ epochSize . config
    childrenCount <- asks $ childrenCount . config
    txt <- asks inputTxt
    statistics <- asks statistics


    families <- forM epoch (\p ->
        do
          let p' = A.delay p
          children <- lift $ mapM (const $ mutate p') [1..childrenCount]

          return $ do
              p <- children
              return $ A.compute @A.P p
      )

    let children = concat families

    let fittest = selectBest epochSize statistics txt children

    return fittest

  where f = id
        order (_,costA) (_,costB) = compare costA costB

