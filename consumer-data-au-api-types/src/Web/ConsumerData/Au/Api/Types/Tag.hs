module Web.ConsumerData.Au.Api.Types.Tag
( OB (OB)
, tagOb
-- Reexports
, Tagged, tagWith
)
where

import           Data.Proxy                 (Proxy (Proxy))
import           Data.Tagged                (Tagged, tagWith)

data OB = OB

tagOb :: a -> Tagged OB a
tagOb = tagWith (Proxy :: Proxy OB)
