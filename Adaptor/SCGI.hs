{-# LANGUAGE OverloadedStrings #-}

module Adaptor.SCGI ( runSCGI, CGI(..), cgiGetHeaders, cgiGetBody,
    writeResponse, sendResponse, sendRedirect, doCGI
  ) where

import Preface
import qualified Data.ByteString as B

{-
import Control.Applicative
import Control.Exception (SomeException, bracket, catch)
import Control.Concurrent ( forkIO, newQSem, waitQSem, signalQSem, MVar, Chan,
    newChan, writeChan, readChan, newEmptyMVar, putMVar, takeMVar, QSem )
import qualified Data.ByteString.Char8 as B
import Network (PortID(..), listenOn)
import Network.Socket (accept, sClose, withSocketsDo, Socket(..) )
import Network.Socket.ByteString (recv, send)
import System.IO (hPutStrLn, stderr)
import Data.List (intersperse)
import System.Environment (getEnvironment)
import Data.Char (digitToInt, isDigit)
import Debug.Trace

import Control.Monad (join, when)
-}

handler :: (CGI -> IO ()) -> QSem -> Socket -> IO ()
handler f qsem socket = do
  waitQSem qsem
  (sock, _) <- sktAccept socket
  _ <- forkIO $ do
      catch (doSCGI f sock) (\e -> hPutStrLn stderr $ "scgi: "++show (e::SomeException))
      signalQSem qsem
  handler f qsem socket

-- | runSCGI threads port cgi-function (in the IO monad)
runSCGI :: Int -> Int -> (CGI -> IO () ) -> IO ()
runSCGI maxThreads port f = join (doListen port <$> handler f <$> newQSem maxThreads )

doSCGI :: ( CGI -> IO() ) -> Socket -> IO ()
doSCGI f sock = do
  a <- getSCGI sock
  f a
  writeResponse a B.empty

-- | WARNING: this ignores non-digits
toInt :: String -> Int
toInt z = foldl (\x y -> 10*x+y) 0 (map digitToInt (filter isDigit z))

netstring :: NetData -> IO (B.ByteString, NetData)
netstring nd =
  let (lenx, rest) = B.break (== fromIntegral (ord ':')) (ndBuf nd)
      lens = asString lenx
      len = toInt lens
   in do
        (res, ndxx) <- ndGet len nd { ndBuf = B.drop 1 rest }
        (_,ndxxx) <- ndGet 1 ndxx
        return (res,ndxxx)

ndGet :: Int -> NetData -> IO (B.ByteString, NetData)
ndGet x nd@(NetData sok buf) =
  if x <= B.length buf then let (bh, bt) = B.splitAt x buf in return (bh, nd { ndBuf = bt } )
  else do
    let y = x - B.length buf
    more <- sktRecv sok (max y 4096)
    if B.length more > 0 then do
      (res, ndx) <-  ndGet y nd { ndBuf = more }
      return ( B.concat [buf, res], ndx )
    else return (ndBuf nd, nd { ndBuf = B.empty})

ndNew :: Socket -> IO NetData
ndNew sock = NetData sock <$> sktRecv sock 4096

-- | Given a socket, sparks a thread to read it, and returns an MVar which can retrieve the
-- CGI variables, and a channel which can retrieve the post body in 4k chunks
getSCGI :: Socket -> IO CGI
getSCGI sock = do
    env <- newEmptyMVar
    chan <- newChan
    _ <- forkIO $ do
        (input, ndx) <- netstring =<< ndNew sock
        let vars = headersx ( asString input)
            len = case lookup "CONTENT_LENGTH" vars of { Nothing -> 0; Just x -> toInt x }
        putMVar env vars
        recurse len ndx chan
    hdrs <- newEmptyMVar
    wchan <- newChan
    _ <- forkIO $ do
        hd <- takeMVar hdrs
        -- I could send the headers immediately and then wait for the body, OR insert into the output stream
        -- sendFully sock hdx
        writeBody wchan sock (genhdrs hd)
    return $ CGI env chan hdrs wchan
  where blksiz = 4096
        writeBody x k buf = do
              buf2 <- sendBlocks k buf 8192
              bb <- readChan x
              if B.length bb == 0 then sendBlocks k buf2 (-1) >> sClose k
              else writeBody x k (B.concat [buf2, bb])
        recurse n nnd ch =
              if n <= 0 then writeChan ch B.empty
              else do (b,nnd')<-ndGet (min blksiz n) nnd
                      writeChan ch b
                      recurse (n- blksiz) nnd' ch
        headersx = pairs . split
        pairs (x:y:xys) = (x, y) : pairs xys
        pairs _ = []
        split str = let (token, rest) = break (== '\NUL') str
                     in if null rest then [token] else  token : split (tail rest)

genhdrs :: HTTPHeaders -> ByteString 
genhdrs hd =  B.concat . intersperse "\r\n" $ [ asByteString (n++": "++v) | (n,v) <- hd] ++ [B.empty,B.empty]

sendRedirect :: CGI -> String -> IO ()
sendRedirect rsp loc = putMVar (cgiRspHeaders rsp) [("Status","302 Found"),("Location",loc)]

sendResponse :: CGI -> HTTPHeaders -> IO ()
sendResponse rsp = putMVar (cgiRspHeaders rsp)

writeResponse :: Stringy a => CGI -> a -> IO ()
writeResponse rsp = writeChan (cgiRspBody rsp) . asByteString

sendBlocks :: Socket -> B.ByteString -> Int -> IO B.ByteString
sendBlocks s bs n = if B.length bs < n then return bs else do
  sent <- sktSend s bs
  let res = B.drop sent bs
  if B.length res == 0 then return res else sendBlocks s res n

doListen :: Int -> (Socket -> IO ()) -> IO ()
doListen port loop = withSocketsDo $ bracket (listenOn (PortNumber (fromIntegral port))) sClose loop

defaultContentType :: String
defaultContentType = "text/html; charset=ISO-8859-1"

data NetData = NetData {
  _ndSocket :: Socket,
  ndBuf :: B.ByteString
} deriving (Show)

type CGIVars = [(String,String)]
type HTTPHeaders = [(String,String)]
type PostBody = B.ByteString

data CGI = CGI { cgiRqHeaders :: MVar CGIVars,  cgiRqBody :: Chan PostBody
                , cgiRspHeaders :: MVar HTTPHeaders, cgiRspBody :: Chan PostBody
               }

cgiGetHeaders :: CGI -> IO CGIVars
cgiGetHeaders = takeMVar . cgiRqHeaders

cgiGetBody :: CGI -> IO B.ByteString
cgiGetBody cgir = cgiGetBody' cgir ""
  where cgiGetBody' cgir sf = do
          x <- readChan (cgiRqBody cgir)
          if B.null x then return sf else cgiGetBody' cgir (B.concat [sf, x])

doCGI :: ( CGI -> IO() ) -> IO ()
doCGI f = do
  a <- getCGI
  f a
  writeResponse a B.empty

writeBlocks :: B.ByteString -> IO ()
writeBlocks = B.putStr

getCGI :: IO CGI
getCGI = do
    env <- newEmptyMVar
    chan <- newChan
    vars <- getEnvironment
    putMVar env vars

    hdrs <- newEmptyMVar
    wchan <- newChan
    _ <- forkIO $ do
        hd <- takeMVar hdrs
        -- I could send the headers immediately and then wait for the body, OR insert into the output stream
        -- sendFully sock hdx
        writeBody wchan (genhdrs hd)
    return $ CGI env chan hdrs wchan
  where blksiz = 4096
        writeBody x buf = do
              writeBlocks buf
              bb <- readChan x
              when (B.length bb > 0) (writeBody x bb)
        recurse n nnd ch = do
              if n <= 0 then writeChan ch B.empty
              else do (b,nnd')<-ndGet (min blksiz n) nnd
                      writeChan ch b
                      recurse (n- blksiz) nnd' ch
