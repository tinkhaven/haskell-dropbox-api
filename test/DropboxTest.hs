module Main where

import Dropbox ( getSessionId
               , getRedirectAuthUrl
               , getAccountInfo)
main = do
    let sID = getSessionId
--    res <- getAccountInfo sID
    (url, session) <- getRedirectAuthUrl sID Nothing
    putStrLn url
    putStrLn "Continue?"
    inputStr <- getLine
    info <- getAccountInfo session
    putStrLn $ show info
