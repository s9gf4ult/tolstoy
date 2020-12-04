module Tolstoy.DSL.JsonPath where

import Tolstoy.Structure.JsonPath

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
