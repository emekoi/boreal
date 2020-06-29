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
import System.Environment (getEnv)
import System.IO.Unsafe (unsafePerformIO)

clientIdB :: B.ByteString
clientIdB = unsafePerformIO $ B.pack <$> getEnv "MAL_CLIENT_ID"
{-# NOINLINE clientIdB #-}

clientIdT :: Text
clientIdT = unsafePerformIO $ T.pack <$> getEnv "MAL_CLIENT_ID"
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
{- mkReq :: MonadIO m => HttpConfig -> Req a -> m a
mkReq = runReq -}
