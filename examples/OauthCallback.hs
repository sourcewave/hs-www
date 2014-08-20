-- {-# LANGUAGE OverloadedStrings #-}

import Adaptor.SCGI
import Preface

-- | command line arguments are:
--   the port to listen on
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
 writeResponse cgir (asByteString (show hdrs))
 writeResponse cgir "\n\n\n"
 writeResponse cgir (strCat ["the post body is ",(show (strLen body))," long\n"])
 writeResponse cgir ""

