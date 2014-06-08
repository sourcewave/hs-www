{-# LANGUAGE OverloadedStrings #-}

import Adaptor.PostgreSQL as PG
import Control.Monad (forever)
import System.Environment (getArgs)
import qualified Data.ByteString as B (pack)
import qualified Data.Text as T (pack, unpack, singleton)
import qualified Data.Text.Encoding as T (encodeUtf8, decodeUtf8)

main :: IO ()
main = do
    args <- getArgs
    
    a <- PG.connectTo "localhost" 5432 
    print "hello"    
    
    b <- doQuery a (StartUpMessage [("user","r0ml"),("database","storewave")] )
    print b
    
    -- c <- doQuery a (Query "select * from pg_tables")
    -- print c
    
    d <- doQuery a (FunctionCall 1598 [])
    print d
    
    e <- doQuery a (Parse "stm" "select * from pg_tables where schemaname = $1" [])
    print e
    
    f <- doQuery a (Bind "clem" "stm" [Just $ (T.encodeUtf8 . T.pack) (head args)])
    print f
    
    g <- doQuery a (Execute "clem" 3)
    print g
    
--    forever $ do
--      h <- getResponse a
--      print h
      
--------------------------------------------------------------------------------