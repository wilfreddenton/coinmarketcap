module Client (
  getTicker
  ) where

import           Data.Proxy
import           Servant.API
import           Servant.Client

import           API
import           Types

coinMarketCapAPI :: Proxy CoinMarketCapAPI
coinMarketCapAPI = Proxy

getTicker :: String -> ClientM Tickers
getTickers :: ClientM Tickers

getTicker :<|> getTickers = client coinMarketCapAPI
