module Day9 where

import Conduit
import Control.Monad.Fail
import Control.Monad.State as S (get, MonadState, put, StateT, runStateT)
import Day5

icp !!! ip = if fromIntegral (length icp) - 1 < ip then 0 else icp !! (fromIntegral ip :: Int)

interpretFromC :: (MonadFail m, MonadIO m, MonadState i m, Integral i) => i -> [i] -> ConduitT i i m [i]
interpretFromC ip icp = case opc'm icp ip of
  (99, _) -> return icp
  (1, ms) -> {- liftIO (printf "%3d ADD (MODE=%s) %d %d %d\n" ip (show ms) (icp!!(ip+1)) (icp!!(ip+2)) (icp!!(ip+3))) >> -}
             eval (+) icp ip ms >>= interpretFromC (ip+4)
  (2, ms) -> {- liftIO (printf "%3d MUL (MODE=%s) %d %d %d\n" ip (show ms) (icp!!(ip+1)) (icp!!(ip+2)) (icp!!(ip+3))) >> -}
             eval (*) icp ip ms >>= interpretFromC (ip+4)
  (3,  _) -> readIn   icp ip    >>= interpretFromC (ip+2)
  (4, ms) -> printOut icp ip ms >>  interpretFromC (ip+2) icp
  (5, ms) -> {- liftIO (printf "%3d JNZ (MODE=%s) %d %d\n"    ip (show ms) (icp!!(ip+1)) (icp!!(ip+2))              ) >> -}
             do nip <- (jmpIf (/=0) icp ip ms)
                interpretFromC nip icp
  (6, ms) -> {- liftIO (printf "%3d JEZ (MODE=%s) %d %d\n"    ip (show ms) (icp!!(ip+1)) (icp!!(ip+2))              ) >> -}
             do nip <- (jmpIf (==0) icp ip ms)
                interpretFromC nip icp
  (7, ms) -> {- liftIO (printf "%3d LE  (MODE=%s) %d %d %d\n" ip (show ms) (icp!!(ip+1)) (icp!!(ip+2)) (icp!!(ip+3))) >> -}
             cmp (<)  icp ip ms >>= interpretFromC (ip+4)
  (8, ms) -> {- liftIO (printf "%3d EQ  (MODE=%s) %d %d %d\n" ip (show ms) (icp!!(ip+1)) (icp!!(ip+2)) (icp!!(ip+3))) >> -}
             cmp (==) icp ip ms >>= interpretFromC (ip+4)
  (9, ms) -> do
     rb <- lift S.get
     mv <- get icp (ip+1) (head ms)
     lift $ put (rb + mv)
     interpretFromC (1+2) icp
  (opc,_) -> {- liftIO (printf "%3d ERROR" ip) >> -} undefined
  where
    eval op icp ip ms  = do
      lv <- get icp (ip+1) (head ms)
      rv <- get icp (ip+2) (ms!!1)
      return . update icp (ip+3) $ lv `op` rv
    readIn  icp ip     = do
      maybeI <- await
      case maybeI of
        Just i  -> do --liftIO $ printf "%3d IN               %d=%d\n" ip (icp!!(ip+1)) i
                      return $ update icp (ip+1) i
        Nothing -> return icp
    printOut icp ip ms = do
      o <- get icp (ip+1) (head ms)
      --liftIO $ printf "%3d OUT (MODE=%s) %d=%d\n"      ip (show ms) (icp!!(ip+1)) o
      yield o
    update icp ip nv =
      if fromIntegral (length icp) - 1 < icp !!! ip
      then
        let len = fromIntegral ((icp !!! ip) - fromIntegral (length icp)) :: Int
            ext = take len $ repeat 0 in
          icp ++ ext ++ [nv]
      else
        let (h, x:t) = splitAt (fromIntegral (icp !!! ip) :: Int) icp in
          h ++ (nv:t)
    deref icp ip = icp !!! (icp !!! ip)
    get icp ip 0 = return $ deref icp ip
    get icp ip 1 = return $ icp !!! ip
    get icp ip 2 = do
      rb <- S.get
      return $ deref icp (rb+ip)
    jmpIf f icp ip ms = do
      v <- get icp (ip+1) (head ms)
      if f v then get icp (ip+2) (ms!!1) else return $ ip+3
    cmp pred icp ip ms   = do
      lv <- (get icp (ip+1) (head ms))
      rv <- (get icp (ip+2) (ms!!1))
      return . update icp (ip+3) $ if lv `pred` rv then 1 else 0

interpretC :: (MonadFail m, MonadIO m, MonadState i m, Integral i) => [i] -> ConduitT i i m [i]
interpretC = interpretFromC 0

day9C :: ConduitT Integer Integer (StateT Integer IO) ()
day9C = liftIO (readICPFrom "data/Day9-input.txt") >>= interpretC >> return ()

test1 :: ConduitT Integer Integer (StateT Integer IO) ()
test1 = return [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99] >>= interpretC >> return ()

test2 :: ConduitT Integer Integer (StateT Integer IO) ()
test2 = return [1102,34915192,34915192,7,4,7,99,0] >>= interpretC >> return ()

test3 :: ConduitT Integer Integer (StateT Integer IO) ()
test3 = return [104,1125899906842624,99] >>= interpretC >> return ()
