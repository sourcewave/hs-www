{-# LANGUAGE OverloadedStrings #-}
import Adaptor.SCGI
import System.Environment
import Data.ByteString.Char8 as B (concat, pack, length)

main :: IO ()
main = do
  args <- getArgs
  let sport:_ = args
      port = (read sport) :: Int
  runSCGI 1 port post

post :: CGI -> IO ()
post cgir = do
 hdrs <- cgiGetHeaders cgir
 print hdrs
 body <- cgiGetBody cgir
 print body

 sendResponse cgir [("Status","200 OK"),("Content-Type","text/plain")]
 writeResponse cgir (B.pack (show hdrs))
 writeResponse cgir "\n\n\n"
 writeResponse cgir (B.concat ["the post body is ",B.pack (show (B.length body))," long","\n"])
 writeResponse cgir ""
 