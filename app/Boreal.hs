module Main
  ( main,
  )
where

-- import qualified Data.ByteString.Char8 as B

-- import Network.HTTP.Req

import Boreal.Anime
import Boreal.Auth
-- import Boreal.Config
import Data.Aeson (Result (..))
import Data.Text (Text)
import qualified Data.Text as T
import Fmt
import qualified Network.API.MAL.Anime as M
import qualified Network.API.MAL.Auth as M
import Network.API.MAL.Types
import Options.Applicative ((<**>), Parser)
import qualified Options.Applicative as O

data Command
  = Login Text Text
  | Search Text
  | List Text
  | Token
  deriving (Show)

login :: Parser Command
login =
  Login
    <$> O.strArgument (O.metavar "USERNAME" <> O.help "your username")
    <*> O.strArgument (O.metavar "PASSWORD" <> O.help "your password")

searchAnime :: Parser Command
searchAnime =
  Search
    <$> O.strArgument (O.metavar "TITLE" <> O.help "title of anime to search for")

listAnime :: Parser Command
listAnime =
  List
    <$> O.strArgument (O.metavar "MAL_ID" <> O.help "the user's MAL id or '@me' for the currently logged in user" <> O.value "@me")

parseOpts :: Parser Command
parseOpts =
  O.hsubparser
    ( O.command "login" (O.info login (O.progDesc "login to your account"))
        <> O.command "search" (O.info searchAnime (O.progDesc "search for an anime"))
        <> O.command "list" (O.info listAnime (O.progDesc "show the anime list for the given user"))
    )
    O.<|> O.hsubparser
      ( O.internal <> O.command "token" (O.info (pure Token) (O.progDesc "displays MAL auth token and re-auths if needed"))
      )

mainBody :: Command -> IO ()
mainBody (Login u p) = do
  M.userAuthenticate u p >>= \case
    Success at -> saveAuthToken at
    Error err -> print err
mainBody (Search t) = do
  l <- M.searchAnime mempty t
  case l of
    Error err -> print err
    Success l' -> do
      if null l'
        then putStrLn $ "no results found for \"" ++ T.unpack t ++ "\""
        else printAnimeList l'
mainBody (List mid) = do
  getAuthToken >>= \case
    Just at@AuthToken {..} -> do
      al <- M.getAnimeList at mempty mid
      case al of
        Error err -> print err
        Success al' -> printAnimeList al'
    _ -> putStrLn "please login first"
mainBody Token = do
  getAuthToken >>= \case
    Just AuthToken {..} -> do
      fmtLn ("Auth Token:\n  Access Token: " +| access_token |+ "\n  Refresh Token: " +| refresh_token |+ "")
    _ -> putStrLn "please login first"

main :: IO ()
main =
  mainBody
    =<< O.execParser
      ( O.info
          (parseOpts <**> O.helper)
          (O.fullDesc <> O.progDesc "a MAL command line client")
      )
