module Main
  ( main,
    mainBody,
  )
where

import Boreal.Anime
import Boreal.Auth
import Boreal.TUI
import Data.Aeson (Result (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Network.API.MAL.Anime as M
import qualified Network.API.MAL.Auth as M
import Network.API.MAL.Constants (fields)
import Network.API.MAL.Types
import Options.Applicative ((<**>), Parser)
import qualified Options.Applicative as O

data Command
  = Login Text Text
  | Search Text
  | List Text
  | Token
  | TUI Text
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

tuiMode :: Parser Command
tuiMode =
  TUI
    <$> O.strArgument (O.metavar "MAL_ID" <> O.help "the user's MAL id or '@me' for the currently logged in user" <> O.value "@me")

parseOpts :: Parser Command
parseOpts =
  O.hsubparser
    ( O.command "login" (O.info login (O.progDesc "login to your account"))
        <> O.command "search" (O.info searchAnime (O.progDesc "search for an anime"))
        <> O.command "list" (O.info listAnime (O.progDesc "show the anime list for the given user"))
        <> O.command "tui" (O.info tuiMode (O.progDesc "launch the TUI interface"))
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
      al <- M.getAnimeList at [fields ["my_list_status"]] mid
      case al of
        Error err -> print err
        Success al' -> printAnimeList al'
    _ -> putStrLn "please login first"
mainBody Token = do
  getAuthToken >>= \case
    Just AuthToken {..} -> do
      mapM_
        putStr
        [ "Auth Token:\n  Access Token: ",
          T.unpack access_token,
          "\n  Refresh Token: ",
          T.unpack refresh_token,
          "\n"
        ]
    _ -> putStrLn "please login first"
mainBody (TUI u) = tuiMain u

main :: IO ()
main =
  O.execParser
    ( O.info
        (parseOpts <**> O.helper)
        (O.fullDesc <> O.progDesc "a MAL command line client")
    )
    >>= mainBody
