module Tolstoy.DSL.JsonPath where

import Tolstoy.Structure.JsonPath

($:) :: StructureQuery r c r
($:) = QueryRoot

(@:) :: StructureQuery r ('Just c) c
(@:) = QueryContext

(?:)
  :: StructureCondition r ('Just ret)
  -> StructureQuery r c ret
  -> StructureQuery r c ret
(?:) cond q = QueryFilter q cond

(.:)
  :: StructurePath inner outer
  -> StructureQuery r c inner
  -> StructureQuery r c outer
(.:) p q = QueryNesting q p
