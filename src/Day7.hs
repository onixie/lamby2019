{-# LANGUAGE LambdaCase #-}

module Day7 where

import Conduit
import Control.Monad.Fail
import Control.Concurrent.STM
import Data.List
import Data.IORef
import Text.Printf
import Day5

readICPFromC :: (MonadIO m, Integral r, Read r) => FilePath -> ConduitT i o m [r]
readICPFromC = liftIO . readICPFrom

interpretFromC :: (MonadFail m, MonadIO m) => Int -> [Int] -> ConduitT Int Int m [Int]
interpretFromC ip icp = case opc'm icp ip of
  (99, _) -> return icp
  (1, ms) -> {- liftIO (printf "%3d ADD (MODE=%s) %d %d %d\n" ip (show ms) (icp!!(ip+1)) (icp!!(ip+2)) (icp!!(ip+3))) >> -}
             eval (+) icp ip ms >>= interpretFromC (ip+4)
  (2, ms) -> {- liftIO (printf "%3d MUL (MODE=%s) %d %d %d\n" ip (show ms) (icp!!(ip+1)) (icp!!(ip+2)) (icp!!(ip+3))) >> -}
             eval (*) icp ip ms >>= interpretFromC (ip+4)
  (3,  _) -> readIn   icp ip    >>= interpretFromC (ip+2)
  (4, ms) -> printOut icp ip ms >>  interpretFromC (ip+2) icp
  (5, ms) -> {- liftIO (printf "%3d JNZ (MODE=%s) %d %d\n"    ip (show ms) (icp!!(ip+1)) (icp!!(ip+2))              ) >> -}
             interpretFromC (jmpIf (/=0) icp ip ms) icp
  (6, ms) -> {- liftIO (printf "%3d JEZ (MODE=%s) %d %d\n"    ip (show ms) (icp!!(ip+1)) (icp!!(ip+2))              ) >> -}
             interpretFromC (jmpIf (==0) icp ip ms) icp
  (7, ms) -> {- liftIO (printf "%3d LE  (MODE=%s) %d %d %d\n" ip (show ms) (icp!!(ip+1)) (icp!!(ip+2)) (icp!!(ip+3))) >> -}
             cmp (<)  icp ip ms >>= interpretFromC (ip+4)
  (8, ms) -> {- liftIO (printf "%3d EQ  (MODE=%s) %d %d %d\n" ip (show ms) (icp!!(ip+1)) (icp!!(ip+2)) (icp!!(ip+3))) >> -}
             cmp (==) icp ip ms >>= interpretFromC (ip+4)
  (opc,_) -> {- liftIO (printf "%3d ERROR" ip) >> -} undefined
  where
    eval op icp ip ms  = return . update icp (ip+3) $ get icp (ip+1) (head ms) `op` get icp (ip+2) (ms!!1)
    readIn  icp ip     = do
      maybeI <- await
      case maybeI of
        Just i  -> do --liftIO $ printf "%3d IN               %d=%d\n" ip (icp!!(ip+1)) i
                      return $ update icp (ip+1) i
        Nothing -> return icp
    printOut icp ip ms = do
      let o = get icp (ip+1) (head ms)
      --liftIO $ printf "%3d OUT (MODE=%s) %d=%d\n"      ip (show ms) (icp!!(ip+1)) o
      yield o
    update icp ip nv  =
      let (h, x:t) = splitAt (icp !! ip) icp in
        h ++ (nv:t)
    deref icp ip = icp !! (icp !! ip)
    get icp ip 0 = deref icp ip
    get icp ip 1 = icp !! ip
    jmpIf f icp ip ms = if f $ get icp (ip+1) (head ms) then get icp (ip+2) (ms!!1) else ip+3
    cmp f icp ip ms   = return . update icp (ip+3) $ if f (get icp (ip+1) (head ms)) (get icp (ip+2) (ms!!1)) then 1 else 0

interpretC :: (MonadFail m, MonadIO m) => [Int] -> ConduitT Int Int m [Int]
interpretC = interpretFromC 0

day5C :: ConduitT Int Int IO ()
day5C = readICPFromC "data/Day5-input.txt" >>= interpretC >> return ()

-- loopAwait = await >>= (\case Nothing -> return ()
--                              Just o  -> {- liftIO (print o) >> -} loopAwait)

day5Part1 = runConduit $ yield 1 .| day5C .| lastC
day5Part2 = runConduit $ yield 5 .| day5C .| lastC

---

feedback :: (Show i, MonadIO m) => i -> ConduitT i i m r -> ConduitT () i m r
feedback init conduit = do
    out <- liftIO $ newTBQueueIO 10
    ret <- liftIO $ newIORef undefined
    lst <- liftIO $ newIORef undefined
    liftIO $ atomically $ writeTBQueue out init
    recv out .| (conduit >>= liftIO . writeIORef ret) .| send lst out
    liftIO (readIORef lst) >>= yield
    liftIO (readIORef ret)
  where
    recv out = do
      v <- liftIO . atomically $ readTBQueue out
--      liftIO $ printf "receive %v" v
      yield v
      recv out
    send lst out = awaitForever $ \i -> liftIO $ do
      writeIORef lst i
--      liftIO $ printf "send back %v" i
      atomically $ writeTBQueue out i

day7 pss soa = sequence $ do
  (a:b:c:d:e:_) <- permutations pss
  return . runConduit $ soa a b c d e .| do { Just o <- await; return o }

amp ps = leftover ps >> day7C
  where
    day7C :: ConduitT Int Int IO ()
    day7C = readICPFromC "data/Day7-input.txt" >>= interpretC >> return ()

soa a b c d e = amp a .| amp b .| amp c .| amp d .| amp e

soa1 i a b c d e = yield i .| soa a b c d e
day7Part1 = day7 [0..4] $ soa1 0

soal i a b c d e = feedback i $ soa a b c d e
day7Part2 = day7 [5..9] $ soal 0
