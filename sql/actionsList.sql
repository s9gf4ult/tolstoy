WITH RECURSIVE result(id, created_at, parrent_id, document, action) AS (
  SELECT id, created_at, parrent_id, document, action
  FROM ^{actions}
  WHERE id = #{actId}
UNION ALL
  SELECT act.id, act.created_at, act.parrent_id, act.document, act.action
  FROM ^{actions} AS act INNER JOIN result AS res
    ON res.parrent_id = act.id
) SELECT * FROM result
