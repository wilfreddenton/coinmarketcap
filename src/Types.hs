{-# LANGUAGE OverloadedStrings #-}

module Types (
  TickerId,
  Ticker,
  Tickers,
  FiatSymbol (..)
  ) where

import           Data.Aeson
import           Data.Text
import           Web.Internal.HttpApiData

type TickerId = String

data Ticker = Ticker
  { tId               :: TickerId,
    tName             :: String,
    tSymbol           :: String,
    tRank             :: String,
    tPriceBtc         :: String,
    tPriceUsd         :: String,
    tPriceJpy         :: Maybe String,
    t24HVolumeUsd     :: String,
    tMarketCapUsd     :: String,
    tMarketCapJpy     :: Maybe String,
    tAvailableSupply  :: String,
    tTotalSupply      :: String,
    tMaxSupply        :: Maybe String,
    tPercentChange1h  :: String,
    tPercentChange24h :: String,
    tPercentChange7d  :: String,
    tLastUpdated      :: String
  } deriving (Eq, Show)

instance FromJSON Ticker where
  parseJSON (Object o) =
    Ticker <$> o .: "id"
           <*> o .: "name"
           <*> o .: "symbol"
           <*> o .: "rank"
           <*> o .: "price_btc"
           <*> o .: "price_usd"
           <*> o .: "price_jpy"
           <*> o .: "24h_volume_usd"
           <*> o .: "market_cap_usd"
           <*> o .: "market_cap_jpy"
           <*> o .: "available_supply"
           <*> o .: "total_supply"
           <*> o .: "max_supply"
           <*> o .: "percent_change_1h"
           <*> o .: "percent_change_24h"
           <*> o .: "percent_change_7d"
           <*> o .: "last_updated"

instance ToJSON Ticker where
  toJSON (Ticker tickerId name symbol rank priceBtc priceUsd
          priceJpy twoFourHVolumeUsd marketCapUsd marketCapJpy
          availableSupply totalSupply maxSupply percentChange1h
          percentChange24h percentChange7d lastUpdated
         ) = object [ "id" .= tickerId
                    , "name" .= name
                    , "symbol" .= symbol
                    , "rank" .= rank
                    , "price_btc" .= priceBtc
                    , "price_usd" .= priceUsd
                    , "price_jpy" .= priceJpy
                    , "24h_volume_usd" .= twoFourHVolumeUsd
                    , "market_cap_usd" .= marketCapUsd
                    , "market_cap_jpy" .= marketCapJpy
                    , "available_supply" .= availableSupply
                    , "total_supply" .= totalSupply
                    , "max_supply" .= maxSupply
                    , "percent_change_1h" .= percentChange1h
                    , "percent_change_24h" .= percentChange24h
                    , "percent_change_7d" .= percentChange7d
                    , "last_updated" .= lastUpdated
                    ]

type Tickers = [Ticker]

data FiatSymbol = AUD | BRL | CAD | CHF | CLP | CNY
                | CZK | DKK | EUR | GBP | HKD | HUF
                | IDR | ILS | INR | JPY | KRW | MXN
                | MYR | NOK | NZD | PHP | PKR | PLN
                | RUB | SEK | SGD | THB | TRY | TWD
                | ZAR deriving (Eq, Show)

instance ToHttpApiData FiatSymbol where
  toQueryParam = pack . show
