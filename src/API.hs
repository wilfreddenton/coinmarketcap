{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module API (
  CoinMarketCapAPI
  ) where

import           Servant.API

import           Types

type CoinMarketCapAPI =
       "ticker" :> Capture "id" String
                :> QueryParam "convert" FiatSymbol
                :> Get '[JSON] Tickers
  :<|> "ticker" :> QueryParam "start" Integer
                :> QueryParam "limit" Integer
                :> QueryParam "convert" FiatSymbol
                :> Get '[JSON] Tickers
