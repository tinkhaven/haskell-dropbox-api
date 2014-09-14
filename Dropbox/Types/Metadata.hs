{- Directive to allow Text and String to be mixed -}
{-# LANGUAGE OverloadedStrings #-}
module Dropbox.Types.Metadata (Metadata(..)) where
import Data.Aeson             ((.:), (.:?), decode, eitherDecode, FromJSON(..), Value(..))
import Control.Applicative    ((<$>), (<*>))
import Data.Attoparsec.Number (Number(..))
import Dropbox.Types.Content
{-
=== METADATA JSON === 
 {
     "size": "0 bytes",
     "hash": "37eb1ba1849d4b0fb0b28caf7ef3af52",
     "bytes": 0,
     "thumb_exists": false,
     "rev": "714f029684fe",
     "modified": "Wed, 27 Apr 2011 22:18:51 +0000",
     "path": "/Public",
     "is_dir": true,
     "icon": "folder_public",
     "root": "dropbox",
     "contents": [
        @see Contents
     ],
     "revision": 29007
 }
=== METADATA JSON === 
 -}

data Metadata       = Metadata { size           :: String
                               , hash           :: String
                               , bytes          :: Int
                               , thumbExists    :: Bool
                               , rev            :: String
                               , modified       :: String
                               , path           :: String
                               , isDir          :: Bool
                               , icon           :: String
                               , root           :: String
                               , contents       :: [Content]
                               } deriving (Show)

instance FromJSON Metadata where
    parseJSON (Object v) = 
        Metadata <$>
        (v .: "size")             <*>
        (v .: "hash")             <*>
        (v .: "bytes")            <*>
        (v .: "thumb_exists")     <*>
        (v .: "rev")              <*>
        (v .: "modified")         <*>
        (v .: "path")             <*>
        (v .: "is_dir")           <*>
        (v .: "icon")             <*>
        (v .: "root")             <*>
        (v .: "contents")
