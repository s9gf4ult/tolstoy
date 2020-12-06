module Tolstoy.DSL.JsonPath.Build where

import Tolstoy.Structure.JsonPath
import Tolstoy.DSL.Render

root :: StructureQuery r c r
root = QueryRoot

ctx :: StructureQuery r ('Just c) c
ctx = QueryContext

(?:)
  :: StructureQuery r c ret
  -> StructureCondition r ('Just ret)
  -> StructureQuery r c ret
(?:) = QueryFilter

infixl 7 ?:

(.:)
  :: StructureQuery r c inner
  -> StructurePath inner outer
  -> StructureQuery r c outer
(.:) = QueryNesting

infixl 8 .:

renderQuery
  :: forall root ret. (Structural root, Structural ret)
  => StructureQuery (StructKind root) 'Nothing (StructKind ret)
  -> Text
renderQuery =
