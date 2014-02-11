module Main where

import Dropbox ( DropboxSession(..)
               , getSession
               , getAccountInfo)
main = do
    session <- getSession Nothing
--    res <- getAccountInfo sID
    let url = getAuthorizationUrl session
    putStrLn url
    putStrLn "Continue?"
    inputStr <- getLine
    info <- getAccountInfo session
    putStrLn $ show info
