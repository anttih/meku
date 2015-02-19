module Kavi.XmlValidation
  ( (?)
  , validateProgram
  ) where

import Control.Alt ((<|>))
import Control.Apply ((*>))
import Control.Plus
import Control.MonadPlus.Partial (mcatMaybes)
import Data.Maybe
import qualified Data.StrMap as M
import Data.Validation
import Data.Either
import Data.Foreign
import Data.Foreign.Class
import Data.String (split)
import Data.String.Regex (regex, test)
import qualified Data.Tuple as T
import Data.Traversable (sequence)
import Data.Argonaut.Core (Json(..))
import Data.Argonaut.Encode (encodeJson)
import Global (readInt)

import Kavi.Types
import Kavi.Xml
import qualified Kavi.Enums as E
import Kavi.Util (all, contains)


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

ok :: forall a. a -> Result a -> Result a
ok default = runV (const $ pure default) pure

requiredAttr :: forall a. Maybe Xml -> String -> Result String
requiredAttr xml name = required (xml </=> name) ? "Pakollinen attribuutti " ++ name ++ " puuttuu."

requiredElement :: Maybe Xml -> String -> Result String
requiredElement p field = required (p </> field) ? "Pakollinen elementti " ++ field ++ " puuttuu"

validateProgram' :: Maybe Xml -> Result Program
validateProgram' p = (Program <$>) $ program
  <$> (p `requiredAttr` "TYPE" *> legacyProgramType)
  <*> externalId
  <*> name
  <*> (requireType *> nameFi)
  <*> optional nameSv
  <*> optional nameOther
  <*> optional year
  <*> countries
  <*> productionCompanies
  <*> required synopsis
  <*> (requireType *> season)
  <*> (requireType *> episode)
  <*> (requireType *> parentTvSeriesName)
  <*> legacyGenre
  <*> directors
  <*> actors
  <*> (required (p <//> "LUOKITTELU") *> classification p)
    where
    typeAttr = p </=> "TYPE"
    requireType = required typeAttr
    externalId = p `requiredElement` "ASIAKKAANTUNNISTE"
    name = p `requiredElement` "ALKUPERAINENNIMI"
    nameSv = p </> "RUOTSALAINENNIMI"
    nameOther = p </> "MUUNIMI"
    year = (p </> "JULKAISUVUOSI") <|> (p </> "VALMISTUMISVUOSI") <#> readInt 10

    countries = validCountries (p </> "MAAT" <#> split " ")
      where
      validCountries :: Maybe [String] -> Result (Maybe [E.CountryCode])
      validCountries Nothing = pure Nothing
      validCountries (Just xs) = Just <$> (sequence $ either fail' pure <<< E.countryCode <$> xs)
        where
        fail' (TypeMismatch _ got) = fail ? "Virheellinen arvo MAAT kentässä: " ++ got
        fail' _ = fail

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
    legacyGenre = const (<>)
      <$> genre E.legacyGenre (p </> "LAJIT") ? "Virheellinen LAJI"
      <*> genre E.legacyTvGenre (p </> "TELEVISIO-OHJELMALAJIT") ? "Virheellinen TELEVISIO-OHJELMALAJIT"
      <*> genre E.legacyGameGenre (p </> "PELINLAJIT") ? "Virheellinen PELINLAJIT"

    directors :: Result [String]
    directors = pure $ mcatMaybes (p </*> "OHJAAJA" <#> fullname)

    actors :: Result [String]
    actors = pure $ mcatMaybes (p </*> "NAYTTELIJA" <#> fullname)

classification :: Maybe Xml -> Result Classification
classification p =
  { duration: _, author: _ , criteria: _, comments: _}
  <$> required duration
  <*> author
  <*> criteria
  <*> comments
  <#> Classification
    where
    duration = p <//> "LUOKITTELU" </> "KESTO"
    author = p `requiredElement` "LUOKITTELIJA"

    criteria :: Result [E.Criteria]
    criteria = sequence $ criteria' <$> (p <//> "LUOKITTELU" </*> "VALITTUTERMI")
      where
      criteria' xml = requiredCriteria *> maybe fail readCriteria (Just xml </=> "KRITEERI")
        where
        requiredCriteria = Just xml `requiredAttr` "KRITEERI"
        readCriteria = either failMsg pure <<< E.criteria
        failMsg (TypeMismatch _ got) = fail ? "Virheellinen KRITEERI: " ++ got

    comments :: Result (M.StrMap String)
    comments = pure $ M.fromList $ mcatMaybes (comment <$> (p <//> "LUOKITTELU" </*> "VALITTUTERMI"))
      where
      comment :: Xml -> Maybe (T.Tuple String String)
      comment xml = let doc = Just xml
        in T.Tuple <$> doc </=> "KRITEERI" <*> doc </=> "KOMMENTTI"


genre :: (String -> F E.LegacyGenre) -> Maybe String -> Result [E.LegacyGenre]
genre _ Nothing = pure []
genre f (Just s) = sequence $ either (const fail) pure <<< f <$> split " " s

fullname :: Xml -> Maybe String
fullname xml = do
  firstname <- Just xml </> "ETUNIMI"
  lastname <- Just xml </> "SUKUNIMI"
  return $ firstname ++ " " ++ lastname

isFormat :: forall a. (a -> Boolean) -> a -> Result a
isFormat f v | f v = pure v
isFormat _ _ = fail ? "Virheellinen kentän formaatti"

isMaybeFormat :: forall a. (a -> Boolean) -> Maybe a -> Result (Maybe a)
isMaybeFormat f (Just v) | f v = pure (Just v)
isMaybeFormat f (Just v) = fail ? "Virheellinen kentän formaatti"
isMaybeFormat f Nothing = pure Nothing

onlyNumbers :: String -> Boolean
onlyNumbers = test $ regex "^\\d+$" {unicode: false, sticky: false, multiline: false, ignoreCase: false, global: false}

validateProgram :: Xml -> Json
validateProgram xml = encodeJson $ validateProgram' (Just xml)
