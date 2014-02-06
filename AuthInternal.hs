{- Directive to allow Text and String to be mixed -}
{-# LANGUAGE OverloadedStrings #-}

module AuthInternal (initSession
                    , getRedirect
                    , getSignedReq
                    ) where

import Data.Conduit (MonadUnsafeIO, MonadResource, MonadBaseControl)
import Network.HTTP.Conduit
import Data.Typeable
import Text.Printf (printf)
import Data.Maybe (fromJust)
--import Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C
import qualified Web.Authenticate.OAuth as OAuth
import Data.Aeson             ((.:), (.:?), decode, eitherDecode, FromJSON(..), Value(..))
import Control.Applicative    ((<$>), (<*>))
import Control.Monad          (liftM)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Attoparsec.Number (Number(..))
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy.Char8 as BS
 
import Dropbox.Types

{- 
    Protocol specification: http://tools.ietf.org/html/rfc5849#section-3.1 
    http://hackage.haskell.org/package/authenticate-0.10.4/docs/Web-Authenticate-OAuth.html#t:Credential

    TODO: https://www.fpcomplete.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/json
        - DeriveGeneric
        - 
-}

server :: String
server = "dropbox"

apiURI :: String
apiURI = "https://api.dropbox.com/1/oauth"

consumerKey :: String
consumerKey = "5aavmwd791ml126"

consumerSecret :: String
consumerSecret = "bjlks0ypu5y4f0y"

{-
Based on sample from
https://github.com/pasberth/ftap/blob/0d39e09194bdb8b32015250c54fd04b3741ed1e2/Ftap/Transfer.hs
 -}
mkConsumer :: B.ByteString -> B.ByteString -> Maybe String -> OAuth.OAuth
mkConsumer key secret callbackURI = OAuth.newOAuth
    { OAuth.oauthServerName = server
    , OAuth.oauthRequestUri = apiURI ++ "/request_token"
    , OAuth.oauthAccessTokenUri = apiURI ++ "/access_token"
    , OAuth.oauthAuthorizeUri = apiURI ++ "/authorize"
    , OAuth.oauthCallback = liftM C.pack callbackURI
    , OAuth.oauthSignatureMethod = OAuth.HMACSHA1
    , OAuth.oauthConsumerKey = key
    , OAuth.oauthConsumerSecret = secret
    , OAuth.oauthVersion         = OAuth.OAuth10a
    }

createOAuth :: Maybe String -> OAuth.OAuth
createOAuth = mkConsumer (C.pack consumerKey) (C.pack consumerSecret)

getSession :: SessionId -> IO DropboxSession
getSession sessionId = initSession sessionId Nothing

--initSession :: SessionId -> Maybe String -> IO SessionId
initSession :: SessionId -> Maybe String -> IO DropboxSession
initSession sessionId callbackUrl = do 
        let oauth = createOAuth callbackUrl
        tmpCred        <- withManager $ OAuth.getTemporaryCredential oauth
        let authRedirectUrl    = OAuth.authorizeUrl oauth tmpCred
            session            = DropboxSession { getSessionId         = sessionId
                                                , getTemporaryToken    = tmpCred 
                                                , getRedirectUrl       = authRedirectUrl
                                                , getOAuth             = oauth
                                                , accessToken          = Nothing
                                                }
        --TODO Persist Session
        --return sessionId
        return session

{-
getRedirect :: SessionId -> String
getRedirect sessionId = 
    let session = getSession sessionId
    in  getRedirectUrl session
-}

getRedirect :: DropboxSession -> String
getRedirect = getRedirectUrl

--getSignedReq :: SessionId -> Request -> IO Request
--getSignedReq sessionId req = do 
getSignedReq :: DropboxSession -> Request -> IO (Request, DropboxSession)
getSignedReq session req = do 
        putStrLn $ "[getSignedReq] with session id " ++ show (getSessionId session) ++ " and req " ++ show req
--        session         <- getSession sessionId
        token           <- getAccessToken session
        putStrLn $ "[getSignedReq] accessToken: " ++ show token
        signedReq       <- OAuth.signOAuth (getOAuth session) token req 
        return (signedReq, session)

--getAccessToken :: (MonadResource m, MonadBaseControl IO m) => DropboxSession -> m OAuth.Credential
getAccessToken session = do
--        putStrLn $ "[getAccessToken] " ++ show (fromJust (accessToken session))
        case accessToken session of
            Just token  ->
                return token
            Nothing     -> do
                token <- withManager $ OAuth.getAccessToken (getOAuth session) (getTemporaryToken session)
                let session = session { accessToken = Just token }
                putStrLn $ "[getAccessToken] Token: " ++ show token
--                putStrLn $ "[getAccessToken] Session: " ++ show session
                return token
                --TODO: Persist in DB

                