{- Directive to allow Text and String to be mixed -}
{-# LANGUAGE OverloadedStrings #-}

module Dropbox.Types ( DropboxSession (..)
                     , AccountInfo
                     , Quota
                     , SessionId
                     ) where 
    
import Web.Authenticate.OAuth (Credential, OAuth)
import Data.Aeson             ((.:), (.:?), decode, eitherDecode, FromJSON(..), Value(..))
import Control.Applicative    ((<$>), (<*>))
import Data.Attoparsec.Number (Number(..))

type SessionId = String

data DropboxSession = DropboxSession { getSessionId       :: SessionId
                                     , getTemporaryToken  :: Credential
                                     , accessToken        :: Maybe Credential
                                     , getOAuth           :: OAuth
                                     , getRedirectUrl     :: String
                                     } deriving Show

{-
=== ACCOUNTINFO JSON ===
{
    "referral_link": "https://www.dropbox.com/referrals/r1a2n3d4m5s6t7",
    "display_name": "John P. User",
    "uid": 12345678,
    "country": "US",
    "quota_info": {
        "shared": 253738410565,
        "quota": 107374182400000,
        "normal": 680031877871
    }
}
=== ACCOUNTINFO JSON ===
 -}
data AccountInfo = AccountInfo { getReferralLink    :: String
                               , getDisplayName     :: String
                               , getUid             :: Number
                               , getCountry         :: String
                               , getQuotaInfo       :: Quota                        
                               } deriving (Show)

data Quota      = Quota        { getQuotaTotal      :: Number
                               , getQuotaNormal     :: Number
                               , getQuotaShared     :: Number
                               } deriving (Show)
                               
instance FromJSON AccountInfo where
    parseJSON (Object v) = 
        AccountInfo <$> 
        (v .: "referral_link")      <*>
        (v .: "display_name")       <*>
        (v .: "uid")                <*>
        (v .: "country")            <*>
        (v .: "quota_info")

instance FromJSON Quota where
    parseJSON (Object v) = 
        Quota <$>
        (v .: "quota")              <*>
        (v .: "normal")             <*>
        (v .: "shared")             

