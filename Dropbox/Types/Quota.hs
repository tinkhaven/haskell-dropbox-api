{- Directive to allow Text and String to be mixed -}
{-# LANGUAGE OverloadedStrings #-}
module Dropbox.Types.Quota (Quota(..)) where
import Data.Aeson             ((.:), (.:?), decode, eitherDecode, FromJSON(..), Value(..))
import Control.Applicative    ((<$>), (<*>))
import Data.Attoparsec.Number (Number(..))
data Quota      = Quota        { getQuotaTotal      :: Number
                               , getQuotaNormal     :: Number
                               , getQuotaShared     :: Number
                               } deriving (Show)

instance FromJSON Quota where
    parseJSON (Object v) = 
        Quota <$>
        (v .: "quota")              <*>
        (v .: "normal")             <*>
        (v .: "shared")             

