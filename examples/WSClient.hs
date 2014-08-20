

--------------------------------------------------------------------------------
import Adaptor.WebSocket  as WS
import Preface

--------------------------------------------------------------------------------
main :: IO ()
main = do
    WS.connectTo "echo.websocket.org" 80 "/" app
  
app :: WebSocket -> IO ()  
app x = do

    putStrLn "Connected!"

    -- Fork a thread that writes WS data to stdout
    _ <- forkIO $ forever $ do
        print "wait for it"
        msg <- WS.wsGetMessage x :: IO ByteString 
        print ("how come?" ++ show msg)

    print "Done"
    -- Read from stdin and write to WS

    let loop = do
            line <- getLine
            unless (null line) $ WS.sendMessage x (Text line) >> loop

    loop
    WS.sendMessage x (Text "Bye!")
    WS.sendMessage x Close

--------------------------------------------------------------------------------
