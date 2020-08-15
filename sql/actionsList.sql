WITH RECURSIVE result(id, created_at, parent_id, document, action) AS (
  SELECT id, created_at, parent_id, document, action
  FROM ^{actions}
  WHERE id = #{actionId}
UNION ALL
  SELECT act.id, act.created_at, act.parent_id, act.document, act.action
  FROM ^{actions} AS act INNER JOIN result AS res
    ON res.parent_id = act.id
) SELECT * FROM result
