module Kavi.Enums where

import Data.Foreign
import Data.Foreign.Class
import Data.Either

foreign import enumValue
  """
  var enums = require('shared/enums');
  function enumValue(type) {
    return function (key) {
      return enums[type] ? enums[type][key] : undefined;
    };
  }
  """ :: String -> String -> Foreign

newtype ProgramType = ProgramType Number

instance isForeignLegacyProgramType :: IsForeign ProgramType where
  read x | isUndefined x = Left $ TypeMismatch "number" "undefined"
  read x = pure $ ProgramType $ unsafeFromForeign x

instance showLegacyProgramType :: Show ProgramType where
  show (ProgramType n) = "ProgramType " ++ case n of
    0 -> "unknown"
    1 -> "movie"
    2 -> "series"
    3 -> "episode"
    4 -> "other-tv"
    5 -> "extra"
    6 -> "trailer"
    7 -> "game"

legacyProgramType :: String -> F ProgramType
legacyProgramType = read <<< enumValue "legacyProgramTypes"

foreign import isCountryCode
  """
  var countries = require('shared/enums').countries;
  function isCountryCode(code) {
    return countries[code] !== undefined;
  }
  """ :: String -> Boolean

newtype LegacyGenre = LegacyGenre String

instance showLegacyGenre :: Show LegacyGenre where
  show (LegacyGenre s) = "LegacyGenre " ++ s

instance isForeignLegacyGenre :: IsForeign LegacyGenre where
  read x | isUndefined x = Left $ TypeMismatch "LAJI" "Virheellinen LAJI"
  read x = pure $ LegacyGenre $ unsafeFromForeign x

legacyGenre :: String -> F LegacyGenre
legacyGenre = read <<< enumValue "legacyGenres"

-- valuesInEnum('LAJIT', enums.legacyGenres),
-- valuesInEnum('TELEVISIO-OHJELMALAJIT', enums.legacyTvGenres),
-- valuesInList('PELINLAJIT', enums.legacyGameGenres)
