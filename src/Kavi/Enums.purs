module Kavi.Enums where

--foreign import enums
--  """
--  var enums = require('shared/enums');
--  """ :: Enums

type ProgramType = Number

foreign import legacyProgramType
  """
  var programTypes = require('shared/enums').programTypes;
  function legacyProgramType(type) {
    programTypes[type];
  }
  """ :: String -> ProgramType

foreign import isLegacyProgramType
  """
  var programTypes = require('shared/enums').programTypes;
  function isLegacyProgramType(type) {
    return programTypes[type] === undefined ? false : true;
  }
  """ :: String -> Boolean

