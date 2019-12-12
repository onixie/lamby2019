{-# LANGUAGE LambdaCase #-}

module Day7 where

import Data.Char
import Text.ParserCombinators.ReadP
import Text.Printf
import Control.Monad.Fail
import Conduit
import Day5

interpretFromC :: (MonadFail m, MonadIO m) => Int -> [Int] -> ConduitT Int Int m [Int]
interpretFromC ip icp = case opc'm icp ip of
  (99, _) -> return icp
  (1, ms) -> liftIO (printf "%3d ADD (MODE=%s) %d %d %d\n" ip (show ms) (icp!!(ip+1)) (icp!!(ip+2)) (icp!!(ip+3))) >> eval (+) icp ip ms >>= interpretFromC (ip+4)
  (2, ms) -> liftIO (printf "%3d MUL (MODE=%s) %d %d %d\n" ip (show ms) (icp!!(ip+1)) (icp!!(ip+2)) (icp!!(ip+3))) >> eval (*) icp ip ms >>= interpretFromC (ip+4)
  (3,  _) -> liftIO (printf "%3d IN               %d="     ip           (icp!!(ip+1))                            ) >> readIn   icp ip    >>= interpretFromC (ip+2)
  (4, ms) -> liftIO (printf "%3d OUT (MODE=%s) %d="        ip (show ms) (icp!!(ip+1))                            ) >> printOut icp ip ms >>  interpretFromC (ip+2) icp
  (5, ms) -> liftIO (printf "%3d JNZ (MODE=%s) %d %d\n"    ip (show ms) (icp!!(ip+1)) (icp!!(ip+2))              ) >> interpretFromC (jmpIf (/=0) icp ip ms) icp
  (6, ms) -> liftIO (printf "%3d JEZ (MODE=%s) %d %d\n"    ip (show ms) (icp!!(ip+1)) (icp!!(ip+2))              ) >> interpretFromC (jmpIf (==0) icp ip ms) icp
  (7, ms) -> liftIO (printf "%3d LE  (MODE=%s) %d %d %d\n" ip (show ms) (icp!!(ip+1)) (icp!!(ip+2)) (icp!!(ip+3))) >> cmp (<)  icp ip ms >>= interpretFromC (ip+4)
  (8, ms) -> liftIO (printf "%3d EQ  (MODE=%s) %d %d %d\n" ip (show ms) (icp!!(ip+1)) (icp!!(ip+2)) (icp!!(ip+3))) >> cmp (==) icp ip ms >>= interpretFromC (ip+4)
  (opc,_) -> liftIO (printf "%3d ERROR" ip)>> undefined
  where
    eval op icp ip ms  = return . update icp (ip+3) $ get icp (ip+1) (head ms) `op` get icp (ip+2) (ms!!1)
    readIn  icp ip     = do
      (Just i) <- await
      liftIO $ print i
      return $ update icp (ip+1) i
    printOut icp ip ms = yield $ get icp (ip+1) (head ms)
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
day5C = liftIO (readICPFrom "data/Day5-input.txt") >>= interpretC >> return ()

loopAwait = await >>= (\case Nothing -> return ()
                             Just o  -> liftIO (print o) >> loopAwait)

day5Part1 = runConduit $ yield 1 .| day5C .| loopAwait
day5Part2 = runConduit $ yield 5 .| day5C .| loopAwait

---

day7C :: ConduitT Int Int IO ()
day7C = liftIO (readICPFrom "data/Day7-input.txt") >>= interpretC >> return ()

amp ps = yield ps .| day7C
