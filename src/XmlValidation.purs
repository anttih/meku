module XmlValidation
  ( Result()
  , (?)
  , Program()
  , Classification()
  , Message()
  , validateProgram
  ) where

import Control.Alt ((<|>))
import Control.Apply ((*>))
import Control.Plus
import Data.Array (findIndex)
import Data.Maybe
import Data.Validation
import Data.Either
import Data.Foreign
import Data.Foreign.Class
import Data.String (split)
import Data.String.Regex (regex, test)
import Global (readInt)

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
  , productionCompanies :: [String]
  , synopsis :: String
  , season :: Maybe String
  , episode :: Maybe String
  , classification :: Classification
  }

program = 
  { programType: _
  , externalId: _
  , name: _
  , nameFi: _
  , nameSv: _
  , nameOther: _
  , year: _
  , countries: _
  , productionCompanies: _
  , synopsis: _
  , season: _
  , episode: _
  , classification: _
  }

type Classification = 
  { duration :: String
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

requiredAttr :: forall a. Maybe Xml -> String -> Result String
requiredAttr xml name = required (xml </=> name) ? "Pakollinen attribuutti " ++ name ++ " puuttuu."

requiredElement :: Maybe Xml -> String -> Result String
requiredElement p field = required (p </> field) ? "Pakollinen elementti " ++ field ++ " puuttuu"

validateProgram' :: Maybe Xml -> Result Program
validateProgram' p = program
  <$> (requiredType *> required (p </=> "TYPE" >>= toLegacyProgramType) ? "Virheellinen attribuutti TYPE")
  <*> p `requiredElement` "ASIAKKAANTUNNISTE"
  <*> p `requiredElement` "ALKUPERAINENNIMI"
  <*> (requiredType >>= validateNameFi)
  <*> optional (p </> "RUOTSALAINENNIMI")
  <*> optional (p </> "MUUNIMI")
  <*> optional ((p </> "JULKAISUVUOSI") <|> (p </> "VALMISTUMISVUOSI") <#> readInt 10)
  <*> validCountries (p </> "MAAT" <#> split " ")
  <*> pure (mcatMaybes $ p </*> "TUOTANTOYHTIO" <#> textContent)
  <*> required (p </> "SYNOPSIS")
  <*> (requiredType >>= validSeason)
  <*> (requiredType >>= validEpisode)
  <*> (required (p <//> "LUOKITTELU") *> validClassification (p <//> "LUOKITTELU"))
    where
    requiredType = p `requiredAttr` "TYPE"

    toLegacyProgramType t | not (t == "05") && isLegacyProgramType t = Just $ legacyProgramType t
    toLegacyProgramType _ = Nothing

    validateNameFi :: String -> Result (Maybe String)
    validateNameFi t = if isAllButTvOrOther t then (Just <$> required nameFi) else optional nameFi
      where
      nameFi = p </> "SUOMALAINENNIMI"
      isAllButTvOrOther = contains ["01","02","03","04","06","07","08","10","11"]

    validCountries :: Maybe [String] -> Result (Maybe [String])
    validCountries (Just names) | all isCountryCode names = pure (Just names)
    validCountries (Just _) = invalid ["Virheellinen kenttä: MAAT"]
    validCountries Nothing = pure Nothing

    validSeason :: String -> Result (Maybe String)
    validSeason t | t == "03" = isValidSeason $ p </> "TUOTANTOKAUSI"
      where
      isValidSeason Nothing = pure Nothing
      isValidSeason (Just t) | onlyNumbers t = pure (Just t)
      isValidSeason (Just t) = invalid ["Virheellinen kenttä: TUOTANTOKAUSI pitää olla numero"]
    validSeason _ = pure Nothing

    validEpisode :: String -> Result (Maybe String)
    validEpisode t | t == "03" = do
      episode <- p `requiredElement` "OSA"
      Just <$> isFormat onlyNumbers episode ? "Virheellinen kenttä: OSA pitää olla numero"
    validEpisode _ = pure Nothing


validClassification :: Maybe Xml -> Result Classification
validClassification xml =
  { duration: _ } <$> required (xml </> "KESTO")

isFormat :: forall a. (a -> Boolean) -> a -> Result a
isFormat f v | f v = pure v
isFormat _ _ = invalid ["Virheellinen kentän formaatti"]

onlyNumbers :: String -> Boolean
onlyNumbers = test $ regex "^\\d+$" {unicode: false, sticky: false, multiline: false, ignoreCase: false, global: true}

all :: forall a. (a -> Boolean) -> [a] -> Boolean
all f []           = true
all f (x:xs) | f x = all f xs
all f _            = false

mcatMaybes :: forall m a. (Monad m, Plus m) => m (Maybe a) -> m a
mcatMaybes m = m >>= maybe empty return

contains :: forall a. (Eq a) => [a] -> a -> Boolean
contains xs x = if findIndex (\v -> v == x) xs == -1 then false else true

validateProgram :: Xml -> Result Program
validateProgram xml = validateProgram' (Just xml)
