module Main where

import Dropbox ( DropboxSession(..)
               , getSession
               , accountInfo
               , metadata)
main = do
    session <- getSession Nothing
--    res <- getAccountInfo sID
    let url = getAuthorizationUrl session
    putStrLn url
    putStrLn "Continue?"
    inputStr <- getLine
    info <- accountInfo session
    putStrLn $ show info
