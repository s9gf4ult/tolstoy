INSERT INTO ^{actionsTable}
  (parent_id, document, document_version, action, action_version)
VALUES
  ( #{parentId}
  , #{JsonField (toStructValue document)}, #{documentVersion}
  , #{JsonField (toStructValue action)}, #{actionVersion} )
RETURNING id, created_at
