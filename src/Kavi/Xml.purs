module Kavi.Xml where

import Data.Maybe
import Data.Either
import Data.Foreign
import Data.Foreign.Class

foreign import data Xml :: *

instance isForeignXml :: IsForeign Xml where
  read x | isXml x = pure $ unsafeFromForeign x
  read _ = Left (TypeMismatch "Xml" "not and XML node")

foreign import isXml
  """
  function isXml(xml) {
    return xml.$name !== undefined;
  }
  """ :: Foreign -> Boolean

foreign import xmlElementText
  """
  function xmlElementText(xml) {
    return function (field) {
      return xml[field] ? xml[field].$text : undefined;
    };
  }
  """ :: Xml -> String -> Foreign

foreign import xmlElement
  """
  function xmlElement(xml) {
    return function (field) {
      return xml[field];
    };
  }
  """ :: Xml -> String -> Foreign

foreign import xmlAttr
  """
  function xmlAttr(xml) {
    return function (field) {
      return xml.$[field];
    };
  }
  """ :: Xml -> String -> Foreign

foreign import xmlChildren
  """
  function xmlChildren(name) {
    return function (xml) {
      return xml.$children.filter(function(e) { return e.$name == name })
    }
  }
  """ :: String -> Xml -> [Xml]

foreign import textContent'
  """
  function textContent$prime(xml) {
    return xml.$text; 
  }
  """ :: Xml -> Foreign

-- Access element value
infixl 5 </>
(</>) :: Maybe Xml -> String -> Maybe String
(</>) doc field = do
  xml <- doc
  either (const Nothing) Just $ read (xmlElementText xml field)

-- Access attribute value
infixl 5 </=>
(</=>) :: Maybe Xml -> String -> Maybe String
(</=>) doc attr = do
  xml <- doc
  either (const Nothing) Just $ read (xmlAttr xml attr)

-- Get children
infixl 5 </*>
(</*>) :: Maybe Xml -> String -> [Xml]
(</*>) (Just xml) field = xmlChildren field xml
(</*>) Nothing _ = []

infixl 5 <//>
(<//>) :: Maybe Xml -> String -> Maybe Xml
(<//>) doc field = do
  xml <- doc
  either (const Nothing) Just $ read (xmlElement xml field)

textContent :: Xml -> Maybe String
textContent xml = either (const Nothing) Just $ read (textContent' xml)
