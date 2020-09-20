-- Returns list of actions recursively. Use "ActionRaw" to parse the result
WITH RECURSIVE result AS (
  SELECT document, document_version, action, id AS action_id, action_version, created_at AS modified, parent_id
  FROM ^{actions}
  WHERE id = #{actionId}
UNION ALL
  SELECT act.document, act.document_version, act.action, act.id AS action_id, act.action_version, act.created_at AS modified, act.parent_id
  FROM ^{actions} AS act INNER JOIN result AS res
    ON res.parent_id = act.id
) SELECT * FROM result
