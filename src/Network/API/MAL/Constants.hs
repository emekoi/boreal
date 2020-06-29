module Network.API.MAL.Constants
  ( clientIdB,
    clientIdT,
    endpointV1,
    endpointV2,
    baseHeaders,
    paging,
    fields,
  )
where

import qualified Data.ByteString.Char8 as B
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Req

clientIdB :: B.ByteString
clientIdB = "6114d00ca681b7701d1e15fe11a4987e"
{-# NOINLINE clientIdB #-}

clientIdT :: Text
clientIdT = "6114d00ca681b7701d1e15fe11a4987e"
{-# NOINLINE clientIdT #-}

endpointV1 :: Url 'Https
endpointV1 = https "api.myanimelist.net" /: "v1"

endpointV2 :: Url 'Https
endpointV2 = https "api.myanimelist.net" /: "v2"

baseHeaders :: Option 'Https
baseHeaders =
  header "X-MAL-Client-ID" clientIdB
    <> httpVersion 2 0

paging :: Int -> Int -> Option 'Https
paging limit offset =
  "limit" =: limit
    <> "offset" =: offset

fields :: [Text] -> Option 'Https
fields f = "fields" =: T.intercalate "," f
