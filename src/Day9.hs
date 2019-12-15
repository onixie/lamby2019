module Day9 where

import Conduit
import Control.Monad.Fail
import Control.Monad.State
import Day5

interpretFromC :: (MonadFail m, MonadIO m, MonadState i m, Integral i) => Int -> [i] -> ConduitT i i m [i]
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
  (9, ms) -> undefined
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

interpretC :: (MonadFail m, MonadIO m, MonadState i m, Integral i) => [i] -> ConduitT i i m [i]
interpretC = interpretFromC 0
