module XmlValidation
  (Result(), Program(), validateProgram)
  where

import Data.Array (findIndex)
import Data.Maybe
import Data.Validation
import Data.Either
import Data.Foreign
import Data.Foreign.Class

import Kavi.Xml
import Kavi.Enums

type Program =
  { programType :: ProgramType
  , externalId :: String
  , name :: String
  , nameFi :: Maybe String
  , nameSv :: Maybe String
  , nameOther :: Maybe String
  , year :: Maybe Number
  , countries :: Maybe [String]
  }

program :: ProgramType
        -> String
        -> String
        -> Maybe String
        -> Maybe String
        -> Maybe String
        -> Maybe Number
        -> Maybe [String]
        -> Program
program programType externalId name nameFi nameSv nameOther year countries =
  { programType: programType
  , externalId: externalId
  , name: name
  , nameFi: nameFi
  , nameSv: nameSv
  , nameOther: nameOther
  , year: year
  , countries: countries
  }

type Message = String
type Result a = V [Message] a

instance bindV :: (Semigroup err) => Bind (V err) where
  (>>=) m f = runV invalid (($) f) m

required :: forall a. Maybe a -> Result a
required Nothing  = invalid []
required (Just v) = pure v

optional :: forall a. Maybe a -> Result (Maybe a)
optional = pure

infixr 5 ?
(?) :: forall a. Result a -> Message -> Result a
(?) res msg = runV (\errors -> invalid (errors <> [msg])) pure res

requiredAttr :: forall a. String -> Maybe Xml -> Result String
requiredAttr name xml = required (xml </=> name) ? "Pakollinen attribuutti " ++ name ++ " puuttuu."

validateProgram :: Maybe Xml -> Result Program
validateProgram p = program
  <$> required (p </=> "TYPE" >>= toLegacyProgramType) ? "Virheellinen attribuutti TYPE"
  <*> required (p </> "ASIAKKAANTUNNISTE")
  <*> required (p </> "ALKUPERAINENNIMI")
  <*> (required (p </=> "TYPE") >>= validateNameFi)
  <*> optional (p </> "RUOTSALAINENNIMI")
  <*> optional (p </> "MUUNIMI")
  <*> optional (p </> "YEAR")
  <*> optional (p </> "MAAT" >>= validateCountries) ? "Virheellinen MAAT kenttä"
    where
    toLegacyProgramType t | not (t == "05") && isLegacyProgramType t = Just (legacyProgramType t)
    toLegacyProgramType _ = Nothing

    validateNameFi :: String -> Result (Maybe String)
    validateNameFi t = if isAllButTvOrOther t then (Just <$> required nameFi) else optional nameFi
      where
      nameFi = p </> "SUOMALAINENNIMI"
      isAllButTvOrOther = contains ["01","02","03","04","06","07","08","10","11"]

    validateCountries xs = Just [xs]

contains :: forall a. (Eq a) => [a] -> a -> Boolean
contains xs x = if findIndex (\v -> v == x) xs == -1 then false else true

--validate :: Xml -> [Result Program]
