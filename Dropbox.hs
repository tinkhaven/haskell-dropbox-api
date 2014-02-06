{- Directive to allow Text and String to be mixed -}
{-# LANGUAGE OverloadedStrings #-}

module Dropbox ( getAccountInfo
               , getSessionId
               , getRedirectAuthUrl
               , AccountInfo(..)
               ) where

import Network.HTTP.Conduit (parseUrl, responseBody, withManager, httpLbs)
import Text.Printf (printf)

import Data.Conduit (MonadUnsafeIO)
import Data.Typeable
import Data.Maybe (fromJust)
--import Data.Text as T
import qualified Data.Char as C
import qualified Data.Word as W
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C
import Data.Aeson             ((.:), (.:?), decode, eitherDecode, FromJSON(..), Value(..))
import Data.Attoparsec.Number (Number(..))
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.UUID as UUID
import qualified Data.UUID.V5 as UUIDv5

import Control.Applicative    ((<$>), (<*>))
import Control.Monad          (liftM)
import Control.Monad.Trans.Resource (runResourceT)

import Dropbox.Types (SessionId, AccountInfo(..))
import Dropbox.Types (DropboxSession)
import AuthInternal (initSession, getRedirect, getSignedReq)

baseUrl :: String
baseUrl = "https://api.dropbox.com/1"

accountInfoUrl :: String
accountInfoUrl = baseUrl ++ "/account/info"

{-
    Create a new session identifier and register the identifier in the database
 -}
getSessionId :: SessionId
getSessionId = 
    let url = map (fromIntegral . C.ord) "api.dropbox.com" :: [W.Word8]
        uuid = UUIDv5.generateNamed UUIDv5.namespaceURL url
    in  UUID.toString $ uuid

{-
    Get the URL where the user can authenticate herself
 -}
getRedirectAuthUrl :: SessionId -> Maybe String -> IO (String, DropboxSession)
getRedirectAuthUrl sessionId callbackUrl = 
    do
        session <- initSession sessionId callbackUrl
        let redirectUrl = getRedirect session
        return $ (redirectUrl, session)
--        sessionId <- initSession sessionId (Just callbackUrl)
--        return $ getRedirect sessionId

--getAccountInfo :: SessionId -> IO (Either String AccountInfo)
getAccountInfo :: DropboxSession -> IO (Either String AccountInfo, DropboxSession)
getAccountInfo session = do
    requestUrl              <- parseUrl accountInfoUrl
    putStrLn $ "[getAccountInfo] " ++ show requestUrl
    putStrLn $ "[getAccountInfo] " ++ show session
    (signedReq, session)    <- getSignedReq session requestUrl
    putStrLn $ "[getAccountInfo]" ++ show signedReq
    accountInfoBody <- responseBody <$> (withManager $ httpLbs signedReq)
    let accountInfo = eitherDecode accountInfoBody :: Either String AccountInfo
    return (accountInfo, session)
