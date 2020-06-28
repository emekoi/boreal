module Main
  ( main,
  )
where

-- import Control.Monad.IO.Class
-- import Data.Aeson
import Data.Aeson.Encode.Pretty
-- import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Stack (HasCallStack)
import qualified Network.API.MAL as M
import Network.API.MAL.Types
-- import Network.HTTP.Req
import Options.Applicative ((<**>), Parser)
import qualified Options.Applicative as O
import System.Directory (XdgDirectory (..), createDirectoryIfMissing, getXdgDirectory)

-- import System.Environment (getEnv)

tshow :: Show a => a -> Text
tshow = T.pack . show

data Command
  = Login Text Text
  | Search Text
  deriving (Show)

-- data RunOpts
--   = RunOpts
--       { optCommand :: Command
--       }

-- main :: IO ()
-- main = do
--   client_id <- B.pack <$> getEnv "MAL_CLIENT_ID"
--   ai <- animeInfo client_id 1
--   L.putStrLn $ encodePretty ai

login :: Parser Command
login =
  Login
    <$> O.strArgument (O.metavar "USERNAME" <> O.help "your username")
    <*> O.strArgument (O.metavar "PASSWORD" <> O.help "your password")

searchAnime :: Parser Command
searchAnime =
  Search
    <$> O.strArgument (O.metavar "TITLE" <> O.help "title of anime")

parseOpts :: Parser Command
parseOpts =
  O.hsubparser
    ( O.command "login" (O.info login (O.progDesc "login to your account"))
        <> O.command "search" (O.info searchAnime (O.progDesc "search for an anime"))
    )

mainBody :: HasCallStack => Command -> IO ()
mainBody (Login u p) = do
  config_dir <- getXdgDirectory XdgConfig "boreal"
  createDirectoryIfMissing True config_dir
  at <- M.userAuthenticate u p
  L.writeFile (config_dir ++ "/config.json") (encodePretty at)
mainBody (Search t) = do
  l <- M.searchAnime t
  print l

-- mainBody x = print x

main :: HasCallStack => IO ()
main =
  mainBody
    =<< O.execParser
      ( O.info
          (parseOpts <**> O.helper)
          (O.fullDesc <> O.progDesc "a MAL command line client")
      )
