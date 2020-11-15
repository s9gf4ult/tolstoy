INSERT INTO ^{documentsTable} (action_id)
VALUES ( #{actionId} )
RETURNING id, created_at
