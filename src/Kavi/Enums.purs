module Kavi.Enums
  ( ProgramType(..)
  , LegacyGenre(..)
  , CountryCode(..)
  , legacyProgramType
  , legacyGenre
  , legacyGameGenre
  , legacyTvGenre
  , countryCode
  ) where

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

newtype CountryCode = CountryCode String

foreign import isCountryCode
  """
  var countries = require('shared/enums').countries;
  function isCountryCode(code) {
    return countries[code] !== undefined;
  }
  """ :: String -> Boolean

countryCode :: String -> F CountryCode
countryCode s | isCountryCode s = pure $ CountryCode s
countryCode s = Left $ TypeMismatch "maakoodi" s

newtype LegacyGenre = LegacyGenre String

instance showLegacyGenre :: Show LegacyGenre where
  show (LegacyGenre s) = "LegacyGenre " ++ s

instance isForeignLegacyGenre :: IsForeign LegacyGenre where
  read x | isUndefined x = Left $ TypeMismatch "Virheellinen LAJI" "LAJI" 
  read x = pure $ LegacyGenre $ unsafeFromForeign x

legacyGenre :: String -> F LegacyGenre
legacyGenre = read <<< enumValue "legacyGenres"

legacyTvGenre :: String -> F LegacyGenre
legacyTvGenre = read <<< enumValue "legacyTvGenres"

foreign import isLegacyGameGenre
  """
  var legacyGameGenres = require('shared/enums').legacyGameGenres;
  function isLegacyGameGenre(val) {
    return legacyGameGenres.indexOf(val) !== -1;
  }
  """ :: String -> Boolean

legacyGameGenre :: String -> F LegacyGenre
legacyGameGenre s | isLegacyGameGenre s = pure $ LegacyGenre s
legacyGameGenre _ = Left $ TypeMismatch "Virheellinen PELINLAJI" "undefined" 
