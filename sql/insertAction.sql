INSERT INTO ^{actionsTable}
  (document, document_version, action, action_version)
VALUES
  ( #{JsonField (toStructValue document)}, #{documentVersion}
  , #{JsonField (toStructValue action)}, #{actionVersion} )
RETURNING id, created_at
