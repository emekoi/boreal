module Main
  ( main,
  )
where

-- import qualified Data.ByteString.Char8 as B

-- import Network.HTTP.Req

import Boreal.Auth
-- import Boreal.Config
import Data.Aeson (Result (..))
import Data.Text (Text)
import qualified Network.API.MAL.Anime as M
import qualified Network.API.MAL.Auth as M
import Options.Applicative ((<**>), Parser)
import qualified Options.Applicative as O

data Command
  = Login Text Text
  | Search Text
  deriving (Show)

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

mainBody :: Command -> IO ()
mainBody (Login u p) = do
  M.userAuthenticate u p >>= \case
    Success at -> saveAuthToken at
    Error err -> print err
mainBody (Search t) = do
  l <- M.searchAnime t
  print l

main :: IO ()
main =
  mainBody
    =<< O.execParser
      ( O.info
          (parseOpts <**> O.helper)
          (O.fullDesc <> O.progDesc "a MAL command line client")
      )
