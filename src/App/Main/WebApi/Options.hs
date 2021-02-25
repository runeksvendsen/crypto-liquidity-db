module App.Main.WebApi.Options
( Options(..)
, withArgs
)
where

import qualified Options.Applicative as Opt
import           Options.Applicative


withArgs :: (Options -> IO a) -> IO a
withArgs f = do
    args <- Opt.execParser opts
    f args

data Options = Options
    { optServerPort :: !Word
    }

opts :: ParserInfo Options
opts = info (helper <*> options) $
     fullDesc
  <> header "cryptomarketdepth.com HTTP API"

options :: Opt.Parser Options
options = Options
    <$> portNum

portNum :: Opt.Parser Word
portNum = option auto $
     long "port"
  <> metavar "PORT"
  <> help "Port number"
  <> value 8000
