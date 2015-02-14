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
import Control.MonadPlus.Partial (mcatMaybes)
import Data.Array (findIndex)
import Data.Maybe
import Data.Validation
import Data.Either
import Data.Foreign
import Data.Foreign.Class
import Data.String (split)
import Data.String.Regex (regex, test)
import Data.Traversable (sequence)
import Global (readInt)

import Data.Foreign

import Kavi.Xml
import qualified Kavi.Enums as E

type Program =
  { programType :: E.ProgramType
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
  , parentTvSeriesName :: Maybe String
  , legacyGenre :: [E.LegacyGenre]
  , directors :: [String]
  , actors :: [String]
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
  , parentTvSeriesName: _
  , legacyGenre: _
  , directors: _
  , actors: _
  , classification: _
  }

type Classification = 
  { duration :: String
  , author :: String
  }

type Message = String
type Result a = V [Message] a

required :: forall a. Maybe a -> Result a
required Nothing  = invalid []
required (Just v) = pure v

optional :: forall a. Maybe a -> Result (Maybe a)
optional = pure

fail :: forall a. Result a
fail = invalid []

-- | Replaces all accumulated errors with one error
infixr 5 ?
(?) :: forall a. Result a -> Message -> Result a
(?) res msg = runV (\_ -> invalid [msg]) pure res

requiredAttr :: forall a. Maybe Xml -> String -> Result String
requiredAttr xml name = required (xml </=> name) ? "Pakollinen attribuutti " ++ name ++ " puuttuu."

requiredElement :: Maybe Xml -> String -> Result String
requiredElement p field = required (p </> field) ? "Pakollinen elementti " ++ field ++ " puuttuu"

validateProgram' :: Maybe Xml -> Result Program
validateProgram' p = program
  <$> (requiredType *> legacyProgramType)
  <*> externalId
  <*> name
  <*> (requiredType *> nameFi)
  <*> optional nameSv
  <*> optional nameOther
  <*> optional year
  <*> countries
  <*> productionCompanies
  <*> required synopsis
  <*> (requiredType *> season)
  <*> (requiredType *> episode)
  <*> (requiredType *> parentTvSeriesName)
  <*> legacyGenre
  <*> directors
  <*> actors
  <*> (required (p <//> "LUOKITTELU") *> classification)
    where
    typeAttr = p </=> "TYPE"
    requiredType = p `requiredAttr` "TYPE"
    externalId = p `requiredElement` "ASIAKKAANTUNNISTE"
    name = p `requiredElement` "ALKUPERAINENNIMI"
    nameSv = p </> "RUOTSALAINENNIMI"
    nameOther = p </> "MUUNIMI"
    year = (p </> "JULKAISUVUOSI") <|> (p </> "VALMISTUMISVUOSI") <#> readInt 10

    countries = validCountries (p </> "MAAT" <#> split " ")
      where
      validCountries :: Maybe [String] -> Result (Maybe [String])
      validCountries (Just names) | all E.isCountryCode names = pure (Just names)
      validCountries (Just _) = invalid ["Virheellinen kenttä: MAAT"]
      validCountries Nothing = pure Nothing

    productionCompanies :: Result [String]
    productionCompanies = pure $ mcatMaybes $ p </*> "TUOTANTOYHTIO" <#> textContent

    synopsis = p </> "SYNOPSIS"

    legacyProgramType :: Result E.ProgramType
    legacyProgramType = case typeAttr of
      Nothing -> fail
      (Just "05") -> fail
      (Just t) -> either (const fail) pure (E.legacyProgramType t) ? "Virheellinen attribuutti TYPE"

    nameFi :: Result (Maybe String)
    nameFi = case typeAttr of
      (Just t) -> if isAllButTvOrOther t
                  then (Just <$> requiredElement p "SUOMALAINENNIMI")
                  else optional $ p </> "SUOMALAINENNIMI"
      Nothing -> fail
        where isAllButTvOrOther = contains ["01","02","03","04","06","07","08","10","11"]

    season :: Result (Maybe String)
    season = case typeAttr of
      (Just "03") -> isMaybeFormat onlyNumbers (p </> "TUOTANTOKAUSI")
                     ? "Virheellinen kenttä: TUOTANTOKAUSI pitää olla numero"
      _ -> pure Nothing

    episode :: Result (Maybe String)
    episode = case typeAttr of
      (Just "03") -> p `requiredElement` "OSA" *> episodeFormat (p </> "OSA")
      _ -> pure Nothing
        where
        episodeFormat Nothing = fail
        episodeFormat (Just episode) = Just <$> isFormat onlyNumbers episode
                                       ? "Virheellinen kenttä: OSA pitää olla numero"

    parentTvSeriesName :: Result (Maybe String)
    parentTvSeriesName = case typeAttr of
      (Just "03") -> p `requiredElement` "ISANTAOHJELMA" <#> Just
      _ -> pure Nothing

    legacyGenre :: Result [E.LegacyGenre]
    legacyGenre = (const (<>))
      <$> genre E.legacyGenre (p </> "LAJIT") ? "Virheellinen LAJI"
      <*> genre E.legacyTvGenre (p </> "TELEVISIO-OHJELMALAJIT") ? "Virheellinen TELEVISIO-OHJELMALAJIT"
      <*> genre E.legacyGameGenre (p </> "PELINLAJIT") ? "Virheellinen PELINLAJIT"

    directors :: Result [String]
    directors = pure $ mcatMaybes (p </*> "OHJAAJA" <#> fullname)

    actors :: Result [String]
    actors = pure $ mcatMaybes (p </*> "NAYTTELIJA" <#> fullname)

    classification :: Result Classification
    classification = let c = p <//> "LUOKITTELU" in
      { duration: _, author: _ }
      <$> required (c </> "KESTO")
      <*> p `requiredElement` "LUOKITTELIJA"

genre :: (String -> F E.LegacyGenre) -> Maybe String -> Result [E.LegacyGenre]
genre _ Nothing = pure []
genre f (Just s) = sequence $ (f' <<< f) <$> split " " s
  where
  f' = either fail pure
    where fail (TypeMismatch _ _) = invalid []

fullname :: Xml -> Maybe String
fullname xml = do
  firstname <- Just xml </> "ETUNIMI"
  lastname <- Just xml </> "SUKUNIMI"
  return $ firstname ++ " " ++ lastname

isFormat :: forall a. (a -> Boolean) -> a -> Result a
isFormat f v | f v = pure v
isFormat _ _ = invalid ["Virheellinen kentän formaatti"]

isMaybeFormat :: forall a. (a -> Boolean) -> Maybe a -> Result (Maybe a)
isMaybeFormat f (Just v) | f v = pure (Just v)
isMaybeFormat f (Just v) = invalid ["Virheellinen kentän formaatti"]
isMaybeFormat f Nothing = pure Nothing

onlyNumbers :: String -> Boolean
onlyNumbers = test $ regex "^\\d+$" {unicode: false, sticky: false, multiline: false, ignoreCase: false, global: false}

all :: forall a. (a -> Boolean) -> [a] -> Boolean
all f []           = true
all f (x:xs) | f x = all f xs
all f _            = false

contains :: forall a. (Eq a) => [a] -> a -> Boolean
contains xs x = if findIndex (\v -> v == x) xs == -1 then false else true

validateProgram :: Xml -> Result Program
validateProgram xml = validateProgram' (Just xml)
