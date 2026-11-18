{-# LANGUAGE OverloadedStrings #-}

-- | Our testsuite, which we run expecting there to be a local Memcached server
-- running on `localhost:11211`.
module Main where

import           MockServer

import qualified Database.Memcache.Client as M
import           Database.Memcache.Cluster (getAliveServers, orderServersForKey)
import           Database.Memcache.Errors
import           Database.Memcache.Server (newServerDefault)
import           Database.Memcache.Socket
import           Database.Memcache.Types

import           Control.Exception
import           Control.Monad
import qualified Data.ByteString.Char8    as BC
import qualified Data.Map.Strict          as Map
import           System.Exit

main :: IO ()
main = do
    putStr "GET:      "
    getTest
    putStrLn "PASSED"
    putStr "DELETE:   "
    deleteTest
    putStrLn "PASSED"
    putStr "RETRY:    "
    retryTest
    putStrLn "PASSED"
    putStr "TIMEOUT1: "
    timeoutTest
    putStrLn "PASSED"
    putStr "TIMEOUT2: "
    timeoutRetryTest
    putStrLn "PASSED"
    putStr "GETMULTI: "
    getFirstMultiTest
    putStrLn "PASSED"
    putStr "GETCHUNKS:"
    getFirstMultiChunksTest
    putStrLn "PASSED"
    putStr "GETMULTI2:"
    getFirstMultiFallbackTest
    putStrLn "PASSED"
    putStr "SETMULTI: "
    setAllMultiTest
    putStrLn "PASSED"
    exitSuccess

getTest :: IO ()
getTest = withMCServer False res $ do
    c <- M.newClient [M.def] M.def
    void $ M.set c (BC.pack "key") (BC.pack "world") 0 0
    Just (v', _, _) <- M.get c "key"
    when (v' /= "world") $ do
        putStrLn $ "bad value returned! " ++ show v'
        exitFailure
  where
    res = [ MR $ emptyRes { resOp = ResSet Loud }
          , MR $ emptyRes { resOp = ResGet Loud "world" 0 }
          ]

deleteTest :: IO ()
deleteTest = withMCServer False res $ do
    c <- M.newClient [M.def] M.def
    v1 <- M.set c "key" "world"  0 0
    v2 <- M.set c "key" "world2" 0 0
    when (v1 == v2) $ do
        putStrLn $ "bad versions! " ++ show v1 ++ ", " ++ show v2
        exitFailure
    r <- M.delete c "key" 0
    unless r $ do
        putStrLn "delete failed!"
        exitFailure
  where
    res = [ MR $ emptyRes { resOp = ResSet Loud, resCas = 1 }
          , MR $ emptyRes { resOp = ResSet Loud, resCas = 2 }
          , MR $ emptyRes { resOp = ResDelete Loud }
          ]

retryTest :: IO ()
retryTest = withMCServer False res $ do
    c <- M.newClient [M.def] M.def
    void $ M.set c (BC.pack "key") (BC.pack "world") 0 0
  where
    res = [ CloseConnection
          , MR $ emptyRes { resOp = ResSet Loud }
          ]

timeoutTest :: IO ()
timeoutTest = withMCServer True res $ do
    c <- M.newClient [M.def] M.def
    void $ M.set c (BC.pack "key") (BC.pack "world") 0 0
    r <- try $ M.get c "key"
    case r of
        Left (ClientError Timeout) -> return ()
        Left  _ -> putStrLn "unexpected exception!" >> exitFailure
        Right _ -> putStrLn "no timeout occured!" >> exitFailure
  where
    res = [ MR $ emptyRes { resOp = ResSet Loud } ]

timeoutRetryTest :: IO ()
timeoutRetryTest = withMCServer False res $ do
    c <- M.newClient [M.def] M.def
    void $ M.set c (BC.pack "key") (BC.pack "world") 0 0
  where
    res = [ DelayMS 800 Noop
          , MR $ emptyRes { resOp = ResSet Loud }
          ]

getFirstMultiTest :: IO ()
getFirstMultiTest = withMCServer False res $ do
    c <- M.newClient [M.def] M.def
    r <- M.getFirstMulti c ["a", "b", "c"]
    let expected = Map.fromList [("a", ("va", 1, 11)), ("c", ("vc", 3, 33))]
    when (r /= expected) $ do
        putStrLn $ "bad result! " ++ show r
        exitFailure
  where
    res = [ MR $ emptyRes { resOp = ResGetK Quiet "a" "va" 1, resCas = 11 }
          , Noop
          , MR $ emptyRes { resOp = ResGetK Quiet "c" "vc" 3, resCas = 33 }
          , MR emptyRes
          ]

getFirstMultiChunksTest :: IO ()
getFirstMultiChunksTest = withMCServer False res $ do
    c <- M.newClient [M.def] M.def
    r <- M.getFirstMulti c ks
    when (r /= expected) $ do
        putStrLn $ "bad result! " ++ show (Map.size r) ++ " keys"
        exitFailure
  where
    ks = BC.pack . show <$> [1 .. 150 :: Int]
    expected = Map.fromList [(k, (k, 0, 0)) | k <- ks]
    hit k = MR $ emptyRes { resOp = ResGetK Quiet k k 0 }
    res = (hit <$> take 100 ks) <> [MR emptyRes] <> (hit <$> drop 100 ks) <> [MR emptyRes]

getFirstMultiFallbackTest :: IO ()
getFirstMultiFallbackTest = do
    c <- M.newClient [spec1, spec2] M.def
    servers <- getAliveServers c
    s1 <- newServerDefault spec1
    let k = head [k' | k' <- BC.pack . show <$> [0 :: Int ..], head (orderServersForKey servers k') == s1]
        hit = [MR $ emptyRes { resOp = ResGetK Quiet k "v" 0, resCas = 7 }, MR emptyRes]
    withMCServerOn "11211" False [Noop, MR emptyRes] $
      withMCServerOn "11212" False hit $ do
        r <- M.getFirstMulti c [k]
        when (r /= Map.fromList [(k, ("v", 0, 7))]) $ do
            putStrLn $ "bad result! " ++ show r
            exitFailure
  where
    spec1 = M.def
    spec2 = M.def { M.ssPort = "11212" }

setAllMultiTest :: IO ()
setAllMultiTest = withMCServer False res $ do
    c <- M.newClient [M.def] M.def
    r <- M.setAllMulti c [("a", "va", 0, 0), ("b", "vb", 0, 0), ("c", "vc", 0, 0)]
    case r of
        [Right [("b", ErrValueTooLarge)]] -> return ()
        _ -> do
            putStrLn $ "bad result! " ++ show r
            exitFailure
  where
    res = [ Noop
          , MR $ emptyRes { resOp = ResSet Quiet, resStatus = ErrValueTooLarge, resOpaque = 1 }
          , Noop
          , MR emptyRes
          ]
