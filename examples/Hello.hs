{-# LANGUAGE OverloadedStrings #-}

import WWW.SCGI
import qualified Data.ByteString.Char8 as B (pack)

cgiMain :: CGI -> IO ()
cgiMain r = do 
   a <- cgiGetHeaders r
   sendResponse r [("Status", "200 OK"), ("Content-Type", "text/html")]
   writeResponse r "<html><body><h1>Hello, from CGI</h1>"
   writeResponse r (B.pack (show a))
   writeResponse r "</body></html>"


--   bd <- getBody
--   setHeader "Content-type" "application/json"
--   output "Hello"

main = doCGI cgiMain

