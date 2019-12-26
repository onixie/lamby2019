{-# LANGUAGE FlexibleContexts #-}
module Day9 where

import Conduit
import Control.Monad
import Control.Monad.Fail
import Control.Monad.State as S (get, MonadState, put, StateT, runStateT)
import Text.Printf
import Day5 (opc'm)
import Day7 (readICPFromC, feedback, day7)
import Prelude hiding (EQ)

icp !!! ip = if fromIntegral (length icp) - 1 < ip then 0 else icp !! (fromIntegral ip :: Int)

data Op = ADD | MUL deriving (Show, Eq)
data Pred = EZ | NZ deriving (Show, Eq)
data Cmp = LE | EQ deriving (Show, Eq)

class (Integral i, PrintfArg i, Show i) => IntCode i
instance IntCode Int
instance IntCode Integer

interpretFromC :: (MonadFail m, MonadIO m, MonadState ic m, IntCode ic, IntCode idx, IntCode i, IntCode o) => idx -> [ic] -> ConduitT i o m [ic]
interpretFromC ip icp = case opc'm icp ip of
  (99, _) -> finish   icp ip
  (1, ms) -> eval ADD icp ip ms >>= interpretFromC (ip+4)
  (2, ms) -> eval MUL icp ip ms >>= interpretFromC (ip+4)
  (3, ms) -> readIn   icp ip ms >>= interpretFromC (ip+2)
  (4, ms) -> printOut icp ip ms >>  interpretFromC (ip+2) icp
  (5, ms) -> jmpIf NZ icp ip ms >>= flip interpretFromC icp
  (6, ms) -> jmpIf EZ icp ip ms >>= flip interpretFromC icp
  (7, ms) -> cmp LE   icp ip ms >>= interpretFromC (ip+4)
  (8, ms) -> cmp EQ   icp ip ms >>= interpretFromC (ip+4)
  (9, ms) -> do
     rb <- lift S.get
     mv <- get icp (ip+1) (head ms)
     lift $ put (rb + mv)
--     liftIO (printf "%3d REL (MODE=%s) %d # old base:%d, new base:%d\n" ip (show ms) (icp!!!(ip+1)) rb (rb+mv))
     interpretFromC (ip+2) icp
  (opc,_) -> liftIO (printf "%3d ERROR" ip) >> undefined
  where
    eval op icp ip ms  = do
      lv <- get icp (ip+1) (head ms)
      rv <- get icp (ip+2) (ms!!1)
--      liftIO (printf "%3d %s (MODE=%s) %d %d %d # arg1=%d arg2=%d\n" ip (show op) (show ms) (icp!!!(ip+1)) (icp!!!(ip+2)) (icp!!!(ip+3)) lv rv)
      update icp (ip+3) (ms!!2) $ case op of
        ADD -> lv + rv
        MUL -> lv * rv
    readIn  icp ip ms  = do
      maybeI <- await
      case maybeI of
        Just i  -> do
--          liftIO $ printf "%3d IN  (MODE=%s) %d # in=%d\n" ip (show ms) (icp!!!(ip+1)) i
          update icp (ip+1) (head ms) (fromIntegral i)
        Nothing -> return icp
    printOut icp ip ms = do
      o <- get icp (ip+1) (head ms)
--      liftIO $ printf "%3d OUT (MODE=%s) %d # out=%d\n"      ip (show ms) (icp!!!(ip+1)) o
      yield $ fromIntegral o
    update icp ip m nv = do
      rb <- S.get
      let pos = (if m == 2 then rb else 0)+(icp!!!ip)
      if fromIntegral (length icp) - 1 < pos
      then do
        let len = fromIntegral (pos - fromIntegral (length icp)) :: Int
        let ext = replicate len 0
        return $ icp ++ ext ++ [nv]
      else do
        let (h, x:t) = splitAt (fromIntegral pos :: Int) icp
        return $ h ++ (nv:t)
    deref icp ip = icp !!! (icp !!! ip)
    get icp ip 0 = return $ deref icp ip
    get icp ip 1 = return $ icp !!! ip
    get icp ip 2 = do
      rb <- S.get
      return $ icp!!!(rb+(icp!!!ip))
    jmpIf pred icp ip ms = do
      v <- get icp (ip+1) (head ms)
--      liftIO (printf "%3d J%s (MODE=%s) %d %d # arg=%d\n" ip (show pred) (show ms) (icp!!!(ip+1)) (icp!!!(ip+2)) v)
      let f = case pred of
                EZ -> (==0)
                NZ -> (/=0)
      if f v then fromIntegral <$> get icp (ip+2) (ms!!1) else return $ ip+3
    cmp pred icp ip ms   = do
      lv <- get icp (ip+1) (head ms)
      rv <- get icp (ip+2) (ms!!1)
--      liftIO (printf "%3d %s  (MODE=%s) %d %d %d # arg1=%d arg2=%d\n" ip (show pred) (show ms) (icp!!!(ip+1)) (icp!!!(ip+2)) (icp!!!(ip+3)) lv rv)
      let cmp = case pred of
                  EQ -> (==)
                  LE  -> (<)
      update icp (ip+3) (ms!!2) $ if lv `cmp` rv then 1 else 0
    finish icp ip = do
--      liftIO (printf "%3d STP\n" ip >> printf "Intcode program:" >> print icp)
      return icp

interpretC :: (MonadFail m, MonadIO m, MonadState ic m, IntCode i, IntCode o, IntCode ic) => [ic] -> ConduitT i o m [ic]
interpretC = interpretFromC (0 :: Integer)

day9C :: ConduitT Integer Integer (StateT Integer IO) ()
day9C = readICPFromC "data/Day9-input.txt" >>= interpretC >> return ()

test1 :: ConduitT Integer Integer (StateT Integer IO) ()
test1 = void $ interpretC [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]

test2 :: ConduitT Integer Integer (StateT Integer IO) ()
test2 = void $ interpretC [1102,34915192,34915192,7,4,7,99,0]

test3 :: ConduitT Integer Integer (StateT Integer IO) ()
test3 = void $ interpretC [104,1125899906842624,99]

run :: Integral i => ConduitT () Void (StateT i IO) r -> IO (r, i)
run = flip runStateT 0 . runConduit

day9Part1 = run $ yield 1 .| day9C .| sinkList

day9Part2 = run $ yield 2 .| day9C .| sinkList

--- Day7 revised

day7C' :: ConduitT Integer Integer (StateT Integer IO) ()
day7C' = readICPFromC "data/Day7-input.txt" >>= interpretC >> return ()

amp' ps = leftover ps >> day7C'

soa' a b c d e = amp' a .| amp' b .| amp' c .| amp' d .| amp' e

soa1' i a b c d e = yield i .| soa' a b c d e
day7Part1' = flip runStateT 0 . day7 [0..4] $ soa1' 0

soal' i a b c d e = feedback i $ soa' a b c d e
day7Part2' = flip runStateT 0 . day7 [5..9] $ soal' 0
