-- Use the "DocDescRaw" to parse rows
SELECT
  act.document as document,
  doc.id as document_id,
  act.document_version,
  act."action" as "action",
  act.id as action_id,
  act.action_version,
  doc.created_at as created,
  act.created_at as modified
  FROM ^{documentsTable} as doc
    INNER JOIN ^{actionsTable} as act ON doc.action_id = act.id
