{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Types
  ( TickerId
  , Ticker
  , Tickers
  , GlobalData
  , FiatSymbol (..)
  ) where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Maybe
import           Data.Text                hiding (drop, filter, map, toLower)
import           Web.Internal.HttpApiData

import           Utils

type TickerId = String

data Ticker = Ticker
  { tId               :: TickerId
  , tName             :: String
  , tSymbol           :: String
  , tRank             :: String
  , tPriceBtc         :: String
  , tPriceUsd         :: String
  , tPriceJpy         :: Maybe String
  , t24HVolumeUsd     :: String
  , tMarketCapUsd     :: String
  , tMarketCapJpy     :: Maybe String
  , tAvailableSupply  :: String
  , tTotalSupply      :: String
  , tMaxSupply        :: Maybe String
  , tPercentChange1h  :: String
  , tPercentChange24h :: String
  , tPercentChange7d  :: String
  , tLastUpdated      :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions
   { fieldLabelModifier = tickerLabelModifier
   , omitNothingFields = True
   } ''Ticker)

type Tickers = [Ticker]

data GlobalData = GlobalData
  { gTotalMarketCapUsd            :: Integer
  , gTotal24hVolumeUsd            :: Integer
  , gBitcoinPercentageOfMarketCap :: Float
  , gActiveCurrencies             :: Integer
  , gActiveAssets                 :: Integer
  , gActiveMarkets                :: Integer
  , gLastUpdated                  :: Integer
  , gTotalMarketCapJpy            :: Maybe Integer
  , gTotal24hVolumeJpy            :: Maybe Integer
  }

$(deriveJSON defaultOptions
   { fieldLabelModifier = globalDataLabelModifier
   , omitNothingFields = True
   } ''GlobalData)

data FiatSymbol = AUD | BRL | CAD | CHF | CLP | CNY
                | CZK | DKK | EUR | GBP | HKD | HUF
                | IDR | ILS | INR | JPY | KRW | MXN
                | MYR | NOK | NZD | PHP | PKR | PLN
                | RUB | SEK | SGD | THB | TRY | TWD
                | ZAR deriving (Eq, Show)

instance ToHttpApiData FiatSymbol where
  toQueryParam = pack . show
