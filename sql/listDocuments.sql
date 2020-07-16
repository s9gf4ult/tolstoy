SELECT
  act.document as doc,
  doc.id as doc_id,
  act.action as act,
  act.id as act_id,
  doc.created_at as created,
  act.created_at as modified
  FROM ^{documents} as doc
    INNER JOIN ^{actions} as act ON doc.action_id = act.id
