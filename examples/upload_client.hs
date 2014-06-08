{-# LANGUAGE OverloadedStrings #-}
import Network
import Network.HTTP.Conduit
import Network.HTTP.Conduit.MultipartFormData

import Data.Text.Encoding as TE
import Data.ByteString.Lazy.Char8

import Control.Monad
import Data.Maybe

main = (withSocketsDo $ withManager $ \m -> do
    zzz <- flip httpLbs m $ fromJust $ parseUrl "http://localhost/"
    flip httpLbs m =<<
        (formDataBody [partBS "title" "Bleaurgh"
                      ,partBS "text" $ TE.encodeUtf8 "çççççççççç"
                --      ,partFileSource "file_1" "/home/friedrich/Photos/MyLittlePony.jpg"
                      ,partFileRequestBody "file_1" "cat.jpg" $ RequestBodyLBS (responseBody zzz)]
            $ fromJust $ parseUrl "http://localhost/run/cgi_upload.py")
    return (responseBody zzz)) >>= Data.ByteString.Lazy.Char8.putStrLn 
