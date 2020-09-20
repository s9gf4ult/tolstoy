CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE TABLE ^{actions} (
  id                 uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
  created_at         timestamp with time zone DEFAULT NOW() NOT NULL,
  parent_id          uuid REFERENCES ^{actions}(id) UNIQUE,
  -- The link to the parent history record. Must be uniq, so all the
  -- history records form a linked list with strict order. If
  -- parent_id is NULL then the record is the first record in the
  -- history list. The head of this list is the latest action
  -- performed to the document. The head is referenced by the
  -- "action_id" in the "documents" table. So the tail is the initial
  -- version of the document
  document           jsonb NOT NULL,
  document_version   bigint NOT NULL,
  -- The version number described in the "versions" table
  "action"           jsonb NOT NULL,
  action_version     bigint NOT NULL
);

CREATE TABLE ^{documents} (
  id                 uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
  created_at         timestamp with time zone DEFAULT NOW() NOT NULL,
  action_id          uuid NOT NULL REFERENCES ^{actions}(id)
  -- The reference to the latest action of the document. The head of
  -- the liked list formed by the "parent_id" reference in the
  -- "actions" table.
);

CREATE TYPE ^{doctypeName} AS ENUM
  ( 'document', 'action' ) ;

CREATE TABLE ^{versions} (
  id                 uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
  created_at         timestamp with time zone DEFAULT NOW() NOT NULL,
  doctype            ^{doctypeName} NOT NULL,
  -- Whether it is a "document" or "action" value version
  "version"          bigint NOT NULL,
  structure_rep      jsonb NOT NULL,
  -- Description of the document structure
  UNIQUE (doctype, "version", structure_rep)
);
