
import Criterion (bench, nf)
import Criterion.Main (defaultMain)

import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy (fromChunks)

import Unsafe.Coerce


import Crypto.Hash.SHA1 as CH
import Data.Digest.Pure.SHA as DD


-- Append this to the end of "Fibber.hs".

hashtests1 = map pack ["Hello, World", "this is a longer string", "yet another string", (take 1000 (repeat (unsafeCoerce 'c')))]

main = defaultMain [
         bench "cryptohash" $ nf (map (\x -> CH.hash x) ) hashtests1
       , bench "data digest" $ nf (map (\x -> bytestringDigest (DD.sha1 (fromChunks [x])))) hashtests1
       ]
       
       
       