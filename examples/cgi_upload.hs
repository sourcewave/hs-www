-- Accepts file uploads and saves the files in the given directory.
-- WARNING: this script is a SECURITY RISK and only for 
-- demo purposes. Do not put it on a public web server.
 
import Network.CGI
import Text.XHtml
 
import qualified Data.ByteString.Lazy as BS
 
import Control.Monad (liftM)
import Data.Maybe (fromJust)
 
uploadDir = "../upload"
 
fileForm = form ! [method "post", enctype "multipart/form-data"]
             << [afile "file", submit "" "Upload"]
saveFile n =
    do cont <- liftM fromJust $ getInputFPS "file"
       let f = uploadDir ++ "/" ++ basename n
       liftIO $ BS.writeFile f cont
       return $ paragraph << ("Saved as " +++ anchor ! [href f] << f +++ ".")
 
page t b = header << thetitle << t +++ body << b
 
basename = reverse . takeWhile (`notElem` "/\\") . reverse
 
cgiMain = 
    do mn <- getInputFilename "file"
       h <- maybe (return fileForm) saveFile mn
       output . renderHtml $ page "Upload example" h
 
main = runCGI $ handleErrors cgiMain
