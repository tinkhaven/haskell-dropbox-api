{- Directive to allow Text and String to be mixed -}
{-# LANGUAGE OverloadedStrings #-}

module Dropbox.Types ( DropboxSession (..)
                     , AccountInfo
                     , Quota
                     , Metadata
                     , Content
                     , SessionId
                     ) where 
    
import Web.Authenticate.OAuth (Credential, OAuth)
import Data.Aeson             ((.:), (.:?), decode, eitherDecode, FromJSON(..), Value(..))
import Control.Applicative    ((<$>), (<*>))
import Data.Attoparsec.Number (Number(..))

import Dropbox.Types.AccountInfo
import Dropbox.Types.Quota
import Dropbox.Types.Content
import Dropbox.Types.Metadata

type SessionId = String

data DropboxSession = DropboxSession { getSessionId         :: SessionId
                                     , getTemporaryToken    :: Credential
                                     , accessToken          :: Maybe Credential
                                     , getOAuth             :: OAuth
                                     , getAuthorizationUrl  :: String
                                     } deriving (Show, Read)