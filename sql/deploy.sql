CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE TABLE ^{actions} (
  id                 uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
  created_at         timestamp with time zone DEFAULT NOW() NOT NULL,
  parent_id          uuid REFERENCES ^{actions}(id) UNIQUE,
  -- The link to the parent history record. Must be uniq, so all the
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

CREATE TABLE ^{versions} (
  id                 uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
  created_at         timestamp with time zone DEFAULT NOW() NOT NULL,
  doctype            text NOT NULL,
  "version"          bigint NOT NULL,
  structure_rep      jsonb NOT NULL,
  -- Description of the document structure
  UNIQUE (doctype, "version", structure_rep)
);
