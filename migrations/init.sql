
CREATE TABLE documents (
  id                 uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
  created_at         timestamp with time zone DEFAULT NOW() NOT NULL,
);

CREATE TABLE actions (
  id                 uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
  document_id        uuid NOT NULL REFERENCES documents(id),
  created_at         timestamp with time zone DEFAULT NOW() NOT NULL,
  seqnum             bigserial NOT NULL UNIQUE,
  -- Seqnum is a seqence number to select the last version of each
  -- document very fast.
  parrent_id         uuid NOT NULL REFERENCES history(id) UNIQUE,
  -- The link to the parrent history record. Must be uniq, so all the
  -- history records form a linked list with strict order. The order
  -- in linked list is more important than order formed by seqnum. The
  -- seqnum is oly for listing documents.
  document           jsonb NOT NULL,
  action             jsonb NOT NULL,
);
