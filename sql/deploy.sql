CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE TABLE ^{actions} (
  id                 uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
  created_at         timestamp with time zone DEFAULT NOW() NOT NULL,
  parrent_id         uuid REFERENCES ^{actions}(id) UNIQUE,
  -- The link to the parrent history record. Must be uniq, so all the
  -- history records form a linked list with strict order. If
  -- parent_id is NULL then the record is the first record in the
  -- history list.
  document           jsonb NOT NULL,
  action             jsonb NOT NULL
);

CREATE TABLE ^{documents} (
  id                 uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
  created_at         timestamp with time zone DEFAULT NOW() NOT NULL,
  action_id          uuid NOT NULL REFERENCES ^{actions}(id)
);
