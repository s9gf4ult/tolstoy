SELECT
  id,
  doctype,
  version,
  structure_rep,
  created_at
FROM ^{versionsTable}
ORDER BY version ASC
