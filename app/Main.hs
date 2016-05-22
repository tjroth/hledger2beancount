{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text          as T
import           Data.Time.Calendar
import           Lib
import           Options.Generic


data App = App {file :: [String] } deriving (Generic, Show)

instance ParseRecord App

main :: IO ()
main = do
  arg <- getRecord "hledger2beancount -- Convert hledger file to beancount"
  print (arg :: App)


  ej <- journalFromFiles (file arg)
  case ej of
    Left _ -> putStrLn "Problem with journal file"
    Right j -> do
      let oa = beanFile openDate j
      putStrLn (T.unpack oa)
  where
      openDate = fromGregorian 1970 1 1
