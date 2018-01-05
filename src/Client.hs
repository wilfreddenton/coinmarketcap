module Client (
  getTicker,
  getTickers
  ) where

import           Data.Proxy
import           Servant.API
import           Servant.Client

import           API
import           Types

coinMarketCapAPI :: Proxy CoinMarketCapAPI
coinMarketCapAPI = Proxy

getTicker :: TickerId -> Maybe FiatSymbol -> ClientM Tickers
getTickers :: Maybe Integer -> Maybe Integer -> Maybe FiatSymbol -> ClientM Tickers
getGlobalData :: Maybe FiatSymbol -> ClientM GlobalData

getTicker :<|> getTickers :<|> getGlobalData = client coinMarketCapAPI
