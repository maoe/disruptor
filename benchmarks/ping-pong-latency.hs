{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
import Control.Applicative
import Control.Concurrent.Chan
import Control.Concurrent.STM
import Control.Monad

import Control.DeepSeq (NFData(..))

import Criterion.Main
import Control.Concurrent.Async

iterations :: Int
iterations = 100

main :: IO ()
main = defaultMain
  [ env setupChan $ \ ~(pingQ, pongQ) ->
      bench "Chan" $ whnfIO $ benchmarkChan pingQ pongQ
  , env setupTChan $ \ ~(pingQ, pongQ) ->
      bench "TChan" $ whnfIO $ benchmarkTChan pingQ pongQ
  , env setupTQueue $ \ ~(pingQ, pongQ) ->
      bench "TQueue" $ whnfIO $ benchmarkTQueue pingQ pongQ
  , env setupTBQueue $ \ ~(pingQ, pongQ) ->
      bench "TBQueue" $ whnfIO $ benchmarkTBQueue pingQ pongQ
  ]

-------------------------------------------------
-- Chan

setupChan :: IO (Chan Int, Chan Int)
setupChan = (,) <$> newChan <*> newChan

benchmarkChan :: Chan Int -> Chan Int -> IO ()
benchmarkChan pingQ pongQ =
  withAsync (pongerChan pingQ pongQ) $ \_ ->
    pingerChan pingQ pongQ iterations

pingerChan
  :: Chan Int -- ^ Ping queue
  -> Chan Int -- ^ Pong queue
  -> Int
  -> IO ()
pingerChan pingQ pongQ = loop
  where
    loop n = when (n > 0) $ do
      writeChan pingQ n
      !_ <- readChan pongQ
      loop $ n - 1

pongerChan
  :: Chan Int -- ^ Ping queue
  -> Chan Int -- ^ Pong queue
  -> IO ()
pongerChan pingQ pongQ = forever $
  readChan pingQ >>= writeChan pongQ

-------------------------------------------------
-- TChan

setupTChan :: IO (TChan Int, TChan Int)
setupTChan = atomically $ (,) <$> newTChan <*> newTChan

benchmarkTChan :: TChan Int -> TChan Int -> IO ()
benchmarkTChan pingQ pongQ =
  withAsync (pongerTChan pingQ pongQ) $ \_ ->
    pingerTChan pingQ pongQ iterations

pingerTChan
  :: TChan Int -- ^ Ping queue
  -> TChan Int -- ^ Pong queue
  -> Int
  -> IO ()
pingerTChan pingQ pongQ = loop
  where
    loop n = when (n > 0) $ do
      atomically $ writeTChan pingQ n
      !_ <- atomically $ readTChan pongQ
      loop $ n - 1

pongerTChan
  :: TChan Int -- ^ Ping queue
  -> TChan Int -- ^ Pong queue
  -> IO ()
pongerTChan pingQ pongQ = forever $
  atomically (readTChan pingQ) >>= atomically . writeTChan pongQ

-------------------------------------------------
-- TQueue

setupTQueue :: IO (TQueue Int, TQueue Int)
setupTQueue = atomically $ (,) <$> newTQueue <*> newTQueue

benchmarkTQueue :: TQueue Int -> TQueue Int -> IO ()
benchmarkTQueue pingQ pongQ =
  withAsync (pongerTQueue pingQ pongQ) $ \_ ->
    pingerTQueue pingQ pongQ iterations

pingerTQueue
  :: TQueue Int -- ^ Ping queue
  -> TQueue Int -- ^ Pong queue
  -> Int
  -> IO ()
pingerTQueue pingQ pongQ = loop
  where
    loop n = when (n > 0) $ do
      atomically $ writeTQueue pingQ n
      !_ <- atomically $ readTQueue pongQ
      loop $ n - 1

pongerTQueue
  :: TQueue Int -- ^ Ping queue
  -> TQueue Int -- ^ Pong queue
  -> IO ()
pongerTQueue pingQ pongQ = forever $
  atomically (readTQueue pingQ) >>= atomically . writeTQueue pongQ

-------------------------------------------------
-- TBQueue

setupTBQueue :: IO (TBQueue Int, TBQueue Int)
setupTBQueue = atomically $ (,) <$> newTBQueue 1024 <*> newTBQueue 1024

benchmarkTBQueue :: TBQueue Int -> TBQueue Int -> IO ()
benchmarkTBQueue pingQ pongQ =
  withAsync (pongerTBQueue pingQ pongQ) $ \_ ->
    pingerTBQueue pingQ pongQ iterations

pingerTBQueue
  :: TBQueue Int -- ^ Ping queue
  -> TBQueue Int -- ^ Pong queue
  -> Int
  -> IO ()
pingerTBQueue pingQ pongQ = loop
  where
    loop n = when (n > 0) $ do
      atomically $ writeTBQueue pingQ n
      !_ <- atomically $ readTBQueue pongQ
      loop $ n - 1

pongerTBQueue
  :: TBQueue Int -- ^ Ping queue
  -> TBQueue Int -- ^ Pong queue
  -> IO ()
pongerTBQueue pingQ pongQ = forever $
  atomically (readTBQueue pingQ) >>= atomically . writeTBQueue pongQ

-------------------------------------------------

instance NFData (Chan a)
instance NFData (TChan a)
instance NFData (TQueue a)
instance NFData (TBQueue a)
