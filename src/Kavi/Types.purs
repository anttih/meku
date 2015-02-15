module Kavi.Types
  ( Program(..)
  , Classification(..)
  , program)
  where

import Data.Maybe
import Data.Argonaut ((~>), (:=), jsonEmptyObject, fromNumber)
import Data.Argonaut.Encode (EncodeJson)

import qualified Kavi.Enums as E

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
  }

instance encodeProgram :: EncodeJson Program where
  encodeJson (Program p)
    = "externalId" := p.externalId
    ~> "nameFi" := p.nameFi
    ~> "nameSv" := p.nameSv
    ~> "nameOther" := p.nameOther
    ~> "year" := p.year
    ~> "synopsis" := p.synopsis
    ~> "season" := p.season
    ~> "episode" := p.episode
    ~> "parentTvSeriesName" := p.parentTvSeriesName
    ~> "classification" := p.classification
    ~> jsonEmptyObject

instance encodeClassification :: EncodeJson Classification where
  encodeJson (Classification c)
    = "duration" := c.duration
    ~> "author" := c.author
    ~> "criteria" := c.criteria
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

