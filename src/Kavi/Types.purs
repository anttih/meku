module Kavi.Types
  ( Program(..)
  , Classification(..)
  , Result()
  , Message()
  , program)
  where

import Data.Maybe
import Data.Argonaut ((~>), (:=), jsonEmptyObject, jsonEmptyArray, fromNumber, fromString)
import Data.Argonaut.Encode (EncodeJson)
import Data.StrMap (StrMap(..))
import Data.Validation

import qualified Kavi.Enums as E

type Message = String
type Result a = V [Message] a

instance encodeV :: EncodeJson (V [String] Program) where
  encodeJson = runV errors res
    where
    errors xs = "errors" := xs ~> "program" := jsonEmptyObject ~> jsonEmptyObject
    res p = "errors" := jsonEmptyArray ~> "program" := p ~> jsonEmptyObject

newtype Program = Program
  { programType :: E.ProgramType
  , externalId :: String
  , name :: String
  , nameFi :: Maybe String
  , nameSv :: Maybe String
  , nameOther :: Maybe String
  , year :: Maybe Number
  , countries :: Maybe [E.CountryCode]
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

newtype Classification = Classification
  { duration :: String
  , author :: String
  , criteria :: [E.Criteria]
  , comments :: StrMap String
  }

instance encodeProgram :: EncodeJson Program where
  encodeJson (Program p)
    = "programType" := p.programType
    ~> "externalId" := p.externalId
    ~> "name" := p.name
    ~> "nameFi" := p.nameFi
    ~> "nameSv" := p.nameSv
    ~> "nameOther" := p.nameOther
    ~> "year" := p.year
    ~> "countries" := p.countries
    ~> "productionCompanies" := p.productionCompanies
    ~> "synopsis" := p.synopsis
    ~> "season" := p.season
    ~> "episode" := p.episode
    ~> "parentTvSeriesName" := p.parentTvSeriesName
    ~> "legacyGenre" := p.legacyGenre
    ~> "directors" := p.directors
    ~> "actors" := p.actors
    ~> "classification" := p.classification
    ~> jsonEmptyObject

instance encodeProgramType :: EncodeJson E.ProgramType where
  encodeJson (E.ProgramType x) = fromNumber x

instance encodeCountryCode :: EncodeJson E.CountryCode where
  encodeJson (E.CountryCode s) = fromString s

instance encodeLegacyGenres :: EncodeJson E.LegacyGenre where
  encodeJson (E.LegacyGenre s) = fromString s

instance encodeClassification :: EncodeJson Classification where
  encodeJson (Classification c)
    = "duration" := c.duration
    ~> "author" := c.author
    ~> "criteria" := c.criteria
    ~> "comments" := c.comments
    ~> jsonEmptyObject

instance encodeCriteria :: EncodeJson E.Criteria where
  encodeJson (E.Criteria s) = fromNumber s

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
