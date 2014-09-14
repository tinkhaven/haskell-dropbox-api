{- Directive to allow Text and String to be mixed -}
{-# LANGUAGE OverloadedStrings #-}
{- Functional dependencies, relax strict type checking rules -}
{-# LANGUAGE FlexibleContexts  #-}

module Dropbox ( getSession
               , accountInfo
               , metadata
               , AccountInfo(..)
               , Metadata(..)
               , DropboxSession(..)
               ) where

import Network.HTTP.Conduit (parseUrl, responseBody, withManager, httpLbs)
import Network.HTTP.Client (HttpException)
import Text.Printf (printf)
import Control.Monad.Primitive (PrimMonad, unsafePrimToPrim)
import Control.Monad.Base (MonadBase, liftBase)

--import Data.Conduit (MonadUnsafeIO)
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
import Control.Monad.IO.Class (MonadIO)
--import qualified Control.Monad.Trans.Resource.Internal as MTR (MonadThrow)
import qualified Control.Monad.Catch                   as MC  (MonadThrow)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Failure (Failure)

import Dropbox.Types (SessionId, AccountInfo(..), Metadata(..))
import Dropbox.Types (DropboxSession(..))
import AuthInternal (initSession, getRedirectAuthUrl, getSignedReq)

root :: String
root = "dropbox"

baseUrl :: String
baseUrl = "https://api.dropbox.com/1"

accountInfoUrl :: String
accountInfoUrl = baseUrl ++ "/account/info"

metadataUrl :: String -> String
metadataUrl all@(x:_) = 
        baseUrl ++ "/metadata/" ++ root ++ path
        where path = 
                case x of '/'   -> all
                          _     -> '/':all

{-
    Create a new session identifier and register the identifier in the database
 -}
generateSessionId :: SessionId
generateSessionId = 
    let url = map (fromIntegral . C.ord) "api.dropbox.com" :: [W.Word8]
        uuid = UUIDv5.generateNamed UUIDv5.namespaceURL url
    in  UUID.toString $ uuid

{-
    Get the session which contains the
    * the authorization URL
    * the session identifier
 -}
getSession :: (MonadBase base m, PrimMonad base, MonadIO m, MonadBaseControl IO m) --, MTR.MonadThrow m) 
		=> Maybe String -> m DropboxSession
getSession callbackUrl = 
    do
        let sessionId = generateSessionId
        session <- initSession sessionId callbackUrl
        return session

accountInfo :: (MonadBase base m, PrimMonad base, MonadIO m, MonadBaseControl IO m, Failure HttpException m, MC.MonadThrow m) --, MTR.MonadThrow m) 
		=> DropboxSession -> m (Either String AccountInfo, DropboxSession)
accountInfo session = do
    requestUrl                  <- parseUrl accountInfoUrl
    (signedReq, newsession)     <- getSignedReq session requestUrl
    accountInfoBody             <- responseBody <$> (withManager $ httpLbs signedReq)
    let accountInfo             = eitherDecode accountInfoBody :: Either String AccountInfo
    return (accountInfo, newsession)
    
metadata ::	(MonadBase base m, PrimMonad base, MonadIO m, MonadBaseControl IO m, Failure HttpException m, MC.MonadThrow m) --, MTR.MonadThrow m) 
		=> DropboxSession -> String -> m (Either String Metadata, DropboxSession)
metadata session path = do
    requestUrl                  <- parseUrl $ metadataUrl path
    (signedReq, newsession)     <- getSignedReq session requestUrl
    metadataBody                <- responseBody <$> (withManager $ httpLbs signedReq)
    let metadata                = eitherDecode metadataBody :: Either String Metadata
    return (metadata, newsession)
