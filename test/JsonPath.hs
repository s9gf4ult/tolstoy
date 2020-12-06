module JsonPath where

import Tolstoy.DSL.JsonPath.Build
import qualified Data.Text.Lazy as TL

t :: TL.Text
t = renderQuery $ root :?
