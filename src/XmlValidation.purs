module XmlValidation
  ( Result()
  , (?)
  , Program()
  , Classification()
  , Message()
  , validateProgram
  , lajit
  ) where

import Control.Alt ((<|>))
import Control.Apply ((*>))
import Control.Plus
import Control.Monad (foldM)
import Control.MonadPlus.Partial (mcatMaybes)
import Data.Array (findIndex)
import Data.Maybe
import Data.Validation
import Data.Either
import Data.Foreign
import Data.Foreign.Class
import Data.String (split)
import Data.String.Regex (regex, test)
import Global (readInt)

import Data.Foreign

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
  , parentTvSeriesName :: Maybe String
  , legacyGenre :: [String]
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

instance bindV :: (Semigroup err) => Bind (V err) where
  (>>=) m f = runV invalid (($) f) m

instance monadV :: (Semigroup err) => Monad (V err)

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
  <*> (requiredType >>= \v -> case v of
      "03" -> isMaybeFormat onlyNumbers (p </> "TUOTANTOKAUSI") ? "Virheellinen kenttä: TUOTANTOKAUSI pitää olla numero"
      _ -> pure Nothing
      )
  <*> (requiredType >>= \v -> case v of
      "03" -> do
        episode <- p `requiredElement` "OSA"
        Just <$> isFormat onlyNumbers episode ? "Virheellinen kenttä: OSA pitää olla numero"
      _ -> pure Nothing
      )
  <*> (requiredType >>= \t -> if t == "03" then p `requiredElement` "ISANTAOHJELMA" <#> Just else pure Nothing)
  <*> pure (toArray (p </> "LAJIT") <> toArray (p </> "TELEVISIO-OHJELMALAJIT") <> toArray (p </> "PELINLAJIT"))
  <*> pure (mcatMaybes (p </*> "OHJAAJA" <#> fullname))
  <*> pure (mcatMaybes (p </*> "NAYTTELIJA" <#> fullname))
  <*> (required (p <//> "LUOKITTELU") *> classification)
    where
    requiredType = p `requiredAttr` "TYPE"

    toLegacyProgramType t | not (t == "05") = either (const Nothing) Just $ legacyProgramType t
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

    fullname :: Xml -> Maybe String
    fullname xml = do
      firstname <- Just xml </> "ETUNIMI"
      lastname <- Just xml </> "SUKUNIMI"
      return $ firstname ++ " " ++ lastname

    classification :: Result Classification
    classification = { duration: _, author: _ }
      <$> required (c </> "KESTO")
      <*> p `requiredElement` "LUOKITTELIJA"
        where c = p <//> "LUOKITTELU"

isFormat :: forall a. (a -> Boolean) -> a -> Result a
isFormat f v | f v = pure v
isFormat _ _ = invalid ["Virheellinen kentän formaatti"]

isMaybeFormat :: forall a. (a -> Boolean) -> Maybe a -> Result (Maybe a)
isMaybeFormat f (Just v) | f v = pure (Just v)
isMaybeFormat f (Just v) = invalid ["Virheellinen kentän formaatti"]
isMaybeFormat f Nothing = pure Nothing

onlyNumbers :: String -> Boolean
onlyNumbers = test $ regex "^\\d+$" {unicode: false, sticky: false, multiline: false, ignoreCase: false, global: true}

all :: forall a. (a -> Boolean) -> [a] -> Boolean
all f []           = true
all f (x:xs) | f x = all f xs
all f _            = false

contains :: forall a. (Eq a) => [a] -> a -> Boolean
contains xs x = if findIndex (\v -> v == x) xs == -1 then false else true

toArray :: Maybe String -> [String]
toArray Nothing = []
toArray (Just s) = split " " s

lajit :: Maybe String -> Result [LegacyGenre]
lajit Nothing = pure []
lajit (Just s) = foldM f [] $ legacyGenre <$> split " " s where
  f :: [LegacyGenre] -> F LegacyGenre -> Result [LegacyGenre]
  f xs = either fail $ pure <<< (: xs)
    where fail (TypeMismatch _ err) = invalid [err]

validateProgram :: Xml -> Result Program
validateProgram xml = validateProgram' (Just xml)
