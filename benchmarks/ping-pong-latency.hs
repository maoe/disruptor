{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
import Control.Applicative
import Control.Concurrent.Chan
import Control.Concurrent.STM
import Control.Monad

import Control.Concurrent.Async
import Control.DeepSeq (NFData(..))
import Criterion.Main
import qualified Control.Concurrent.Chan.Unagi as U
import qualified Control.Concurrent.Chan.Unagi.Unboxed as UU

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
  , env setupUnagi $ \ ~(pingQ, pongQ) ->
      bench "Unagi" $ whnfIO $ benchmarkUnagi pingQ pongQ
  , env setupUnagiUnboxed $ \ ~(pingQ, pongQ) ->
      bench "Unagi Unboxed" $ whnfIO $ benchmarkUnagiUnboxed pingQ pongQ
  ]

-------------------------------------------------
-- Chan

setupChan :: IO (Chan Int, Chan Int)
setupChan = (,) <$> newChan <*> newChan

benchmarkChan :: Chan Int -> Chan Int -> IO ()
benchmarkChan pingQ pongQ =
  withAsync (pongerChan pingQ pongQ iterations) $ \_ ->
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
  -> Int
  -> IO ()
pongerChan pingQ pongQ = loop
  where
    loop n = when (n > 0) $ do
      readChan pingQ >>= writeChan pongQ
      loop $ n - 1

-------------------------------------------------
-- TChan

setupTChan :: IO (TChan Int, TChan Int)
setupTChan = atomically $ (,) <$> newTChan <*> newTChan

benchmarkTChan :: TChan Int -> TChan Int -> IO ()
benchmarkTChan pingQ pongQ =
  withAsync (pongerTChan pingQ pongQ iterations) $ \_ ->
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
  -> Int
  -> IO ()
pongerTChan pingQ pongQ = loop
  where
    loop n = when (n > 0) $ do
      atomically (readTChan pingQ) >>= atomically . writeTChan pongQ
      loop $ n - 1

-------------------------------------------------
-- TQueue

setupTQueue :: IO (TQueue Int, TQueue Int)
setupTQueue = atomically $ (,) <$> newTQueue <*> newTQueue

benchmarkTQueue :: TQueue Int -> TQueue Int -> IO ()
benchmarkTQueue pingQ pongQ =
  withAsync (pongerTQueue pingQ pongQ iterations) $ \_ ->
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
  -> Int
  -> IO ()
pongerTQueue pingQ pongQ = loop
  where
    loop n = when (n > 0) $ do
      atomically (readTQueue pingQ) >>= atomically . writeTQueue pongQ
      loop $ n - 1

-------------------------------------------------
-- TBQueue

setupTBQueue :: IO (TBQueue Int, TBQueue Int)
setupTBQueue = atomically $ (,) <$> newTBQueue 1024 <*> newTBQueue 1024

benchmarkTBQueue :: TBQueue Int -> TBQueue Int -> IO ()
benchmarkTBQueue pingQ pongQ =
  withAsync (pongerTBQueue pingQ pongQ iterations) $ \_ ->
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
  -> Int
  -> IO ()
pongerTBQueue pingQ pongQ = loop
  where
    loop n = when (n > 0) $ do
      atomically (readTBQueue pingQ) >>= atomically . writeTBQueue pongQ
      loop $ n - 1

-------------------------------------------------
-- Unagi

setupUnagi
  :: IO ((U.InChan Int, U.OutChan Int), (U.InChan Int, U.OutChan Int))
setupUnagi = (,) <$> U.newChan <*> U.newChan

benchmarkUnagi
  :: (U.InChan Int, U.OutChan Int)
  -> (U.InChan Int, U.OutChan Int)
  -> IO ()
benchmarkUnagi (pingInQ, pingOutQ) (pongInQ, pongOutQ) =
  withAsync (pongerUnagi pingOutQ pongInQ iterations) $ \_ ->
    pingerUnagi pingInQ pongOutQ iterations

pingerUnagi
  :: U.InChan Int
  -> U.OutChan Int
  -> Int
  -> IO ()
pingerUnagi pingQ pongQ = loop
  where
    loop n = when (n > 0) $ do
      U.writeChan pingQ n
      !_ <- U.readChan pongQ
      loop $ n - 1

pongerUnagi
  :: U.OutChan Int
  -> U.InChan Int
  -> Int
  -> IO ()
pongerUnagi pingQ pongQ = loop
  where
    loop n = when (n > 0) $ do
      U.readChan pingQ >>= U.writeChan pongQ
      loop $ n - 1

-------------------------------------------------
-- Unagi Unboxed

setupUnagiUnboxed
  :: IO ((UU.InChan Int, UU.OutChan Int), (UU.InChan Int, UU.OutChan Int))
setupUnagiUnboxed = (,) <$> UU.newChan <*> UU.newChan

benchmarkUnagiUnboxed
  :: (UU.InChan Int, UU.OutChan Int)
  -> (UU.InChan Int, UU.OutChan Int)
  -> IO ()
benchmarkUnagiUnboxed (pingInQ, pingOutQ) (pongInQ, pongOutQ) = do
  withAsync (pongerUnagiUnboxed pingOutQ pongInQ iterations) $ \_ ->
    pingerUnagiUnboxed pingInQ pongOutQ iterations

pingerUnagiUnboxed
  :: UU.InChan Int
  -> UU.OutChan Int
  -> Int
  -> IO ()
pingerUnagiUnboxed pingQ pongQ = loop
  where
    loop n = when (n > 0) $ do
      UU.writeChan pingQ n
      !_ <- UU.readChan pongQ
      loop $ n - 1

pongerUnagiUnboxed
  :: UU.OutChan Int
  -> UU.InChan Int
  -> Int
  -> IO ()
pongerUnagiUnboxed pingQ pongQ = loop
  where
    loop n = when (n > 0) $ do
      UU.readChan pingQ >>= UU.writeChan pongQ
      loop $ n - 1

-------------------------------------------------

instance NFData (Chan a)
instance NFData (TChan a)
instance NFData (TQueue a)
instance NFData (TBQueue a)
instance NFData (U.InChan a)
instance NFData (U.OutChan a)
instance NFData (UU.InChan a)
instance NFData (UU.OutChan a)
