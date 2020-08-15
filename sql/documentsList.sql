SELECT
  act.document as document,
  doc.id as document_id,
  act."action" as "action",
  act.id as action_id,
  doc.created_at as created,
  act.created_at as modified
  FROM ^{documents} as doc
    INNER JOIN ^{actions} as act ON doc.action_id = act.id
