module Kavi.Enums where

--foreign import enums
--  """
--  var enums = require('shared/enums');
--  """ :: Enums

type ProgramType = Number

foreign import legacyProgramType
  """
  var programTypes = require('shared/enums').legacyProgramTypes;
  function legacyProgramType(type) {
    return legacyProgramTypes[type];
  }
  """ :: String -> ProgramType

foreign import isLegacyProgramType
  """
  var legacyProgramTypes = require('shared/enums').legacyProgramTypes;
  function isLegacyProgramType(type) {
    return legacyProgramTypes[type] === undefined ? false : true;
  }
  """ :: String -> Boolean

foreign import isCountryCode
  """
  var countries = require('shared/enums').countries;
  function isCountryCode(code) {
    return countries[code] !== undefined;
  }
  """ :: String -> Boolean
