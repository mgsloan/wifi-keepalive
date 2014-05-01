{-# LANGUAGE ScopedTypeVariables #-}

-- | Check the Wifi, if it's misbehaving, restart it.
--
-- Worst case scenario, run:
-- retry system "modprobe -r b43"
-- retry system "modprobe b43"

module Main where

import Control.Concurrent
import Control.Exception (catch,IOException)
import Control.Monad
import Data.Maybe
import Prelude hiding (catch)
import Safe
import System.IO
import System.Process
import System.Timeout

-- | Main entry point.
main :: IO ()
main = do
  pingavg <- getPingAverage
  case readMay pingavg :: Maybe Float of
    Nothing -> do restart
                  sleep wait
                  main
    Just{} -> do sleep 1
                 main

-- | Awful but it turns out the ping command isn't actually that
-- awesome for getting what you want. The timeouts are also
-- insufficient, hence the gnarly timeout code in 'getCmdLine'.
getPingAverage :: IO String
getPingAverage =
  getCmdLine limit
   ("ping -w " ++
    show limit ++
    " -W " ++
    show limit ++
    " -c 1 google.it | grep ^rtt | sed -r 's/.* ([0-9\\.]+)\\/.*$/\\1/'")

-- | When to restart testing after a reconnect.
wait :: Int
wait = 30

-- | How many seconds to wait for a ping reply.
limit :: Int
limit = 3

-- | Restart the wireless connection.
restart :: IO ()
restart = do
  system "nmcli nm wifi off"
  system "nmcli nm wifi on"
  return ()

-- | Sleep for n seconds.
sleep :: Int -> IO ()
sleep n = do
  threadDelay (1000 * 1000 * n)

-- | Call an interactive command, returning empty string on an
-- exception after limit seconds.
getCmdLine :: Int -> String -> IO String
getCmdLine limit string =
  catch (do (_in,out,_err,pid) <- runInteractiveCommand string
            line <- catch (fmap (fromMaybe "")
                                (timeout (1000 * 1000 * limit)
                                         (hGetLine out)))
                          (const (return "") :: IOException -> IO String)
            terminateProcess pid
            waitForProcess pid
            return line)
        (\(e::IOException) -> do print e
                                 return "")
