{- Directive to allow Text and String to be mixed -}
{-# LANGUAGE OverloadedStrings #-}
module Dropbox.Types.Content (Content(..)) where
import Data.Aeson             ((.:), (.:?), decode, eitherDecode, FromJSON(..), Value(..))
import Control.Applicative    ((<$>), (<*>))
import Data.Attoparsec.Number (Number(..))
{-
         {
             "size": "0 bytes",
             "rev": "35c1f029684fe",
             "thumb_exists": false,
             "bytes": 0,
             "modified": "Mon, 18 Jul 2011 20:13:43 +0000",
             "client_mtime": "Wed, 20 Apr 2011 16:20:19 +0000",
             "path": "/Public/latest.txt",
             "is_dir": false,
             "icon": "page_white_text",
             "root": "dropbox",
             "mime_type": "text/plain",
             "revision": 220191
         }
 -}
data Content        = Content  { size           :: String
                               , rev            :: String
                               , thumbExists    :: Bool
                               , bytes          :: Int
                               , modified       :: String
                               , clientMTime    :: String
                               , path           :: String
                               , isDir          :: Bool
                               , icon           :: String
                               , root           :: String
                               , mimeType       :: String
                               } deriving (Show)
                               
instance FromJSON Content where
    parseJSON (Object v) = 
        Content <$>
        (v .: "size")             <*>
        (v .: "rev")              <*>
        (v .: "thumb_exists")     <*>
        (v .: "bytes")            <*>
        (v .: "modified")         <*>
        (v .: "client_mtime")     <*>
        (v .: "path")             <*>
        (v .: "is_dir")           <*>
        (v .: "icon")             <*>
        (v .: "root")             <*>
        (v .: "mime_type")
