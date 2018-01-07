module Utils
  ( tickerLabelModifier
  , globalDataLabelModifier
  ) where

import           Data.Char       (toLower)
import           Data.List       (isPrefixOf)
import           Data.List.Utils (replace)
import           Text.Casing     (fromHumps, toSnake)

tickerLabelModifier :: String -> String
tickerLabelModifier label =
  let label' = modifier label
  in case label' of
    "24_hvolume_usd"    -> "24h_volume_usd"
    "percent_change1h"  -> "percent_change_1h"
    "percent_change24h" -> "percent_change_24h"
    "percent_change7d"  -> "percent_change_7d"
    _                   -> label'

globalDataLabelModifier :: String -> String
globalDataLabelModifier label
    | prefix `isPrefixOf` label' = replace prefix "total_24" label'
    | otherwise = label'
    where label' = modifier label
          prefix = "total24"

modifier :: String -> String
modifier = (map toLower) . toSnake . fromHumps . (drop 1)
