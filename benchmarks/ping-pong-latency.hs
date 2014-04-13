{-# LANGUAGE BangPatterns #-}
import Control.Concurrent.Chan
import Control.Concurrent.STM
import Control.Monad

import Criterion.Main
import Control.Concurrent.Async

iterations :: Int
iterations = 1000

main :: IO ()
main = defaultMain
  [ bench "Chan" $ whnfIO benchmarkChan
  , bench "TChan" $ whnfIO benchmarkTChan
  , bench "TQueue" $ whnfIO benchmarkTQueue
  , bench "TBQueue" $ whnfIO benchmarkTBQueue
  ]

-------------------------------------------------
-- Chan

benchmarkChan :: IO ()
benchmarkChan = do
  pingQ <- newChan
  pongQ <- newChan
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

benchmarkTChan :: IO ()
benchmarkTChan = do
  pingQ <- newTChanIO
  pongQ <- newTChanIO
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

benchmarkTQueue :: IO ()
benchmarkTQueue = do
  pingQ <- newTQueueIO
  pongQ <- newTQueueIO
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

benchmarkTBQueue :: IO ()
benchmarkTBQueue = do
  pingQ <- newTBQueueIO 1024
  pongQ <- newTBQueueIO 1024
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
