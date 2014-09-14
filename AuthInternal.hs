{- Directive to allow Text and String to be mixed -}
{-# LANGUAGE OverloadedStrings #-}
{- Functional dependencies, relax strict type checking rules -}
{-# LANGUAGE FlexibleContexts  #-}

module AuthInternal ( initSession
                    , getRedirectAuthUrl
                    , getSignedReq
                    , mkConsumer
                    ) where

--import Data.Conduit --(MonadUnsafeIO, MonadResource, MonadBaseControl)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Primitive (PrimMonad, unsafePrimToPrim)
import Control.Monad.Base (MonadBase, liftBase)
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
import Control.Monad.IO.Class (MonadIO)
--import Control.Monad.Trans.Resource.Internal (MonadThrow)
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

getSession :: (MonadBase base m, PrimMonad base, MonadIO m, MonadBaseControl IO m) 
		=> String -> m DropboxSession
getSession sessionId = initSession sessionId Nothing

initSession  :: (MonadBase base m, PrimMonad base, MonadIO m, MonadBaseControl IO m) 
		=> String -> Maybe String -> m DropboxSession
initSession sessionId callbackUrl = do 
        let oauth = createOAuth callbackUrl
        tmpCred        <- withManager $ OAuth.getTemporaryCredential oauth
        let authRedirectUrl    = OAuth.authorizeUrl oauth tmpCred
            session            = DropboxSession { getSessionId         = sessionId
                                                , getTemporaryToken    = tmpCred 
                                                , getAuthorizationUrl  = authRedirectUrl
                                                , getOAuth             = oauth
                                                , accessToken          = Nothing
                                                }
        return session

getRedirectAuthUrl :: DropboxSession -> String
getRedirectAuthUrl = getAuthorizationUrl

getAccessToken :: (MonadBase base m, PrimMonad base, MonadIO m, MonadBaseControl IO m)
		=> DropboxSession -> m OAuth.Credential
getAccessToken session = do
        case accessToken session of
            Just token  ->
                return token
            Nothing     -> do
                token <- withManager $ OAuth.getAccessToken (getOAuth session) (getTemporaryToken session)
                let session = session { accessToken = Just token }
                return token

getSignedReq :: (MonadBase base m, PrimMonad base, MonadIO m, MonadBaseControl IO m) 
		=> DropboxSession -> Request -> m (Request, DropboxSession)
getSignedReq session req = do 
        token           <- getAccessToken session
        let newsession  = DropboxSession    { getSessionId         = getSessionId session
                                            , getTemporaryToken    = getTemporaryToken session
                                            , getAuthorizationUrl  = getAuthorizationUrl session
                                            , getOAuth             = getOAuth session
                                            , accessToken          = Just token
                                            }
        signedReq       <- OAuth.signOAuth (getOAuth newsession) token req 
        return (signedReq, newsession)
