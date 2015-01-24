module Kavi.Xml where

import Data.Maybe
import Data.Either
import Data.Foreign
import Data.Foreign.Class

foreign import data Xml :: *

foreign import xmlElement
  """
  function xmlElement(xml) {
    return function (field) {
      return xml[field] ? xml[field].$text : undefined;
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

-- Access element value
infixl 5 </>
(</>) :: Maybe Xml -> String -> Maybe String
(</>) doc field = do
  xml <- doc
  either (const Nothing) Just $ read (xmlElement xml field)

-- Access attribute value
infixl 5 </=>
(</=>) :: Maybe Xml -> String -> Maybe String
(</=>) doc attr = do
  xml <- doc
  either (const Nothing) Just $ read (xmlAttr xml attr)

foreign import testDoc
  """
  var testDoc = {
    "JULKAISUVUOSI": {$text: "2007"}
  };
  """ :: Xml
