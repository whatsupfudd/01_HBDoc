-- KMS — PostgreSQL schema v2

CREATE EXTENSION IF NOT EXISTS citext;
CREATE EXTENSION IF NOT EXISTS pg_trgm;
CREATE EXTENSION IF NOT EXISTS btree_gin;
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE SCHEMA IF NOT EXISTS kms;
-- SET search_path = kms, public;

-- -----------------------------
-- ENUMS
-- -----------------------------
DO $$ BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_type t join pg_namespace n on t.typnamespace = n.oid WHERE n.nspname = 'kms' and typname = 'principal_kind') THEN
    CREATE TYPE kms.principal_kind AS ENUM ('user','group','role','org');
  END IF;
  IF NOT EXISTS (SELECT 1 FROM pg_type t join pg_namespace n on t.typnamespace = n.oid WHERE n.nspname = 'kms' and typname = 'scope_kind') THEN
    CREATE TYPE kms.scope_kind AS ENUM ('tenant','domaintb','bundle','resource');
  END IF;
END $$;

-- -----------------------------
-- 1) Organizations, Users, Groups, Roles
-- -----------------------------
CREATE TABLE IF NOT EXISTS kms.organization (
  uid INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  name TEXT NOT NULL UNIQUE,
  kind TEXT NOT NULL CHECK (kind IN ('internal','law_firm','regulator','auditor','vendor','other')),
  created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE TABLE IF NOT EXISTS kms.roletb (
  uid INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  code TEXT NOT NULL UNIQUE,      -- 'PMO','Sponsor/Executive','Counsel','Domain Owner','Auditor/Regulator'
  name TEXT NOT NULL,
  description TEXT
);

CREATE TABLE IF NOT EXISTS kms.grouptb (
  uid INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  org_fk INT REFERENCES kms.organization(uid) ON DELETE SET NULL,
  code TEXT NOT NULL UNIQUE,      -- 'PMO','CorpSec','Compliance','Tax','Structuring','Capital Markets'
  name TEXT NOT NULL,
  kind TEXT NOT NULL CHECK (kind IN ('internal','cross-entity')),
  created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE TABLE IF NOT EXISTS kms.usertb (
  uid INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY
  , org_fk INT REFERENCES kms.organization(uid) ON DELETE SET NULL
  , email CITEXT NOT NULL UNIQUE
  , full_name TEXT NOT NULL
  , display_name text
  , is_external BOOLEAN NOT NULL DEFAULT FALSE
  , status TEXT NOT NULL DEFAULT 'active' CHECK (status IN ('active','suspended','disabled'))
  , created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE TABLE IF NOT EXISTS kms.group_member (
  group_fk INT NOT NULL REFERENCES kms.grouptb(uid) ON DELETE CASCADE,
  user_fk INT NOT NULL REFERENCES kms.usertb(uid) ON DELETE CASCADE,
  PRIMARY KEY (group_fk, user_fk)
);

-- -----------------------------
-- 2) Catalogs (Domain / Type / Tier / Status)
-- -----------------------------
CREATE TABLE IF NOT EXISTS kms.domaintb (
  uid INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  code TEXT NOT NULL UNIQUE,      -- 'Governance','Regulatory','Tax','Compliance','Structuring','Capital Markets'
  name TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS kms.doc_type (
  uid INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  code TEXT NOT NULL UNIQUE,      -- 'Memo','Policy','Template','Agreement','Register','Procedure'
  name TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS kms.tier (
  uid INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  code TEXT NOT NULL UNIQUE,      -- 'Public','Internal','Restricted','Secret'
  name TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS kms.statustb (
  uid INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  code TEXT NOT NULL UNIQUE,      -- 'Idea','Draft','Review','Counsel','Approved','Final'
  name TEXT NOT NULL,
  order_index INT NOT NULL DEFAULT 0
);

-- -----------------------------
-- 3) RBAC + ABAC overlay
-- -----------------------------
CREATE TABLE IF NOT EXISTS kms.permission (
  uid INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  code TEXT NOT NULL UNIQUE              -- 'view','comment','edit','export','finalize','share','assign_workflow','ai_use','admin'
);

INSERT INTO kms.permission(code) VALUES
  ('view'), ('comment'), ('edit'), ('export'), ('finalize'),
  ('share'), ('assign_workflow'), ('ai_use'), ('admin')
ON CONFLICT DO NOTHING;

CREATE TABLE IF NOT EXISTS kms.role_permission (
  role_fk INT NOT NULL REFERENCES kms.roletb(uid) ON DELETE CASCADE,
  permission_fk INT NOT NULL REFERENCES kms.permission(uid) ON DELETE CASCADE,
  PRIMARY KEY (role_fk, permission_fk)
);

-- Role bindings attach roles to principals with an optional ABAC scope.
CREATE TABLE IF NOT EXISTS kms.role_binding (
  uid INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  principal kms.principal_kind NOT NULL,
  user_fk INT REFERENCES kms.usertb(uid) ON DELETE CASCADE,
  group_fk INT REFERENCES kms.grouptb(uid) ON DELETE CASCADE,
  org_fk INT REFERENCES kms.organization(uid) ON DELETE CASCADE,
  role_fk INT NOT NULL REFERENCES kms.roletb(uid) ON DELETE CASCADE,
  scope kms.scope_kind NOT NULL DEFAULT 'tenant',
  scope_value TEXT,                       -- domaintb.code / bundle.code / document.uid
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CONSTRAINT chk_rb_one_principal CHECK (
    (user_fk IS NOT NULL)::int + (group_fk IS NOT NULL)::int + (org_fk IS NOT NULL)::int = 1
  ),
  CONSTRAINT chk_rb_principal_match CHECK (
    (principal='user' AND user_fk IS NOT NULL) OR
    (principal='group' AND group_fk IS NOT NULL) OR
    (principal='org' AND org_fk IS NOT NULL)
  )
);
create unique index unique_role_binding on kms.role_binding(coalesce(user_fk,-1), coalesce(group_fk,-1), coalesce(org_fk,-1), role_fk, scope, scope_value);


-- -----------------------------
-- 4) Documents & versions
-- -----------------------------

-- noteTaking original def:
-- create table basicdoc (
--   uid INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY
--   , eid uuid not null DEFAULT uuid_generate_v4()
--   -- Basic title in a given locale; translations are stored in a support table.
--   , title varchar(255) not null
--   , locale varchar(8) not null
--   , created_at timestamptz not null default now()
-- );
--

CREATE TABLE IF NOT EXISTS kms.document (
  uid INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY
  , eid uuid not null DEFAULT public.uuid_generate_v4()
  , code TEXT UNIQUE
  , title TEXT NOT NULL
  , domain_fk INT NOT NULL REFERENCES kms.domaintb(uid) ON DELETE RESTRICT
  , doc_type_fk INT NOT NULL REFERENCES kms.doc_type(uid) ON DELETE RESTRICT
  , tier_fk INT NOT NULL REFERENCES kms.tier(uid) ON DELETE RESTRICT
  , status_fk INT NOT NULL REFERENCES kms.statustb(uid) ON DELETE RESTRICT
  , owner_user_fk INT REFERENCES kms.usertb(uid) ON DELETE SET NULL
  , residency TEXT              -- 'EU','UAE','US' etc.
  , ai_allowed BOOLEAN NOT NULL DEFAULT TRUE
  , legal_hold BOOLEAN NOT NULL DEFAULT FALSE
  , due_date DATE
  , created_by_user_fk INT REFERENCES kms.usertb(uid) ON DELETE SET NULL
  , created_at TIMESTAMPTZ NOT NULL DEFAULT now()
  , updated_at TIMESTAMPTZ NOT NULL DEFAULT now()
  , deleted_at TIMESTAMPTZ
);

CREATE TABLE IF NOT EXISTS kms.document_version (
  uid INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  document_fk INT NOT NULL REFERENCES kms.document(uid) ON DELETE CASCADE,
  version_no INT NOT NULL,
  note TEXT,
  content_ref TEXT,               -- object storage pointer (node-level tree)
  content_text TEXT,               -- denormalized text for FTS / preview
  content_sha256 TEXT,
  created_by_user_fk INT REFERENCES kms.usertb(uid) ON DELETE SET NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  is_snapshot BOOLEAN NOT NULL DEFAULT TRUE,
  UNIQUE (document_fk, version_no)
);

CREATE OR REPLACE VIEW kms.v_document_latest AS
SELECT DISTINCT ON (dv.document_fk)
  d.uid AS document_uid,
  d.title, d.domain_fk, d.doc_type_fk, d.tier_fk, d.status_fk,
  d.owner_user_fk, d.residency, d.ai_allowed, d.legal_hold, d.due_date,
  d.created_at, d.updated_at,
  dv.uid AS document_version_uid,
  dv.version_no, dv.note, dv.content_ref, dv.content_text, dv.content_sha256, dv.created_at AS version_created_at
FROM kms.document d
JOIN kms.document_version dv ON dv.document_fk = d.uid
WHERE d.deleted_at IS NULL
ORDER BY dv.document_fk, dv.version_no DESC;

ALTER TABLE kms.document_version ALTER COLUMN content_text SET STORAGE EXTENDED;
CREATE INDEX IF NOT EXISTS idx_docver_content_fts ON kms.document_version USING GIN (to_tsvector('english', coalesce(content_text,'')));

-- Tags
CREATE TABLE IF NOT EXISTS kms.tagtb (
  uid INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  name TEXT NOT NULL UNIQUE
);

CREATE TABLE IF NOT EXISTS kms.document_tag (
  document_fk INT NOT NULL REFERENCES kms.document(uid) ON DELETE CASCADE,
  tag_fk INT NOT NULL REFERENCES kms.tagtb(uid) ON DELETE CASCADE,
  PRIMARY KEY (document_fk, tag_fk)
);

-- Relations (canonical ordering)
CREATE TABLE IF NOT EXISTS kms.document_relation (
  uid INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  document_a_fk INT NOT NULL REFERENCES kms.document(uid) ON DELETE CASCADE,
  document_b_fk INT NOT NULL REFERENCES kms.document(uid) ON DELETE CASCADE,
  relation_type TEXT NOT NULL DEFAULT 'related',  -- 'related','refers_to','derived_from','supersedes','cites'
  directed BOOLEAN NOT NULL DEFAULT FALSE,
  created_by_user_fk INT REFERENCES kms.usertb(uid) ON DELETE SET NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CONSTRAINT chk_rel_canonical CHECK (document_a_fk <> document_b_fk)
  -- Implemented as unique index:
  -- CONSTRAINT uq_rel UNIQUE (LEAST(tdocument_a_fk, document_b_fk), GREATEST(tdocument_a_fk, document_b_fk), relation_type)
);
create unique index unique_document_relation on kms.document_relation (LEAST(document_a_fk, document_b_fk), GREATEST(document_a_fk, document_b_fk), relation_type);

-- Comments
CREATE TABLE IF NOT EXISTS kms.commenttb (
  uid INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  document_fk INT NOT NULL REFERENCES kms.document(uid) ON DELETE CASCADE,
  parent_comment_fk INT REFERENCES kms.commenttb(uid) ON DELETE CASCADE,
  author_user_fk INT REFERENCES kms.usertb(uid) ON DELETE SET NULL,
  body TEXT NOT NULL,
  resolved BOOLEAN NOT NULL DEFAULT FALSE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

-- Attachments
CREATE TABLE IF NOT EXISTS kms.attachment (
  uid INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  document_fk INT NOT NULL REFERENCES kms.document(uid) ON DELETE CASCADE,
  filename TEXT NOT NULL,
  content_type TEXT,
  size_bytes BIGINT,
  storage_ref TEXT NOT NULL,
  sha256 TEXT,
  uploaded_by_user_fk INT REFERENCES kms.usertb(uid) ON DELETE SET NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

-- Document-level ACLs
CREATE TABLE IF NOT EXISTS kms.document_acl (
  uid INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  document_fk INT NOT NULL REFERENCES kms.document(uid) ON DELETE CASCADE,
  principal kms.principal_kind NOT NULL,
  user_fk INT REFERENCES kms.usertb(uid) ON DELETE CASCADE,
  group_fk INT REFERENCES kms.grouptb(uid) ON DELETE CASCADE,
  role_fk INT REFERENCES kms.roletb(uid) ON DELETE CASCADE,
  org_fk INT REFERENCES kms.organization(uid) ON DELETE CASCADE,
  rights TEXT[] NOT NULL,            -- subset of permission.code
  scope kms.scope_kind,                 -- NULL | 'domaintb' | 'bundle' | 'resource'
  scope_value TEXT,
  created_by_user_fk INT REFERENCES kms.usertb(uid) ON DELETE SET NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  CONSTRAINT chk_acl_one_principal CHECK (
    (user_fk IS NOT NULL)::int + (group_fk IS NOT NULL)::int + (role_fk IS NOT NULL)::int + (org_fk IS NOT NULL)::int = 1
  ),
  CONSTRAINT chk_acl_principal_match CHECK (
    (principal='user' AND user_fk IS NOT NULL) OR
    (principal='group' AND group_fk IS NOT NULL) OR
    (principal='role' AND role_fk IS NOT NULL) OR
    (principal='org' AND org_fk IS NOT NULL)
  )
);
CREATE INDEX IF NOT EXISTS idx_docacl_doc ON kms.document_acl(document_fk);
CREATE INDEX IF NOT EXISTS idx_docacl_user ON kms.document_acl(user_fk);
CREATE INDEX IF NOT EXISTS idx_docacl_group ON kms.document_acl(group_fk);
CREATE INDEX IF NOT EXISTS idx_docacl_role ON kms.document_acl(role_fk);
CREATE INDEX IF NOT EXISTS idx_docacl_org ON kms.document_acl(org_fk);

-- -----------------------------
-- 5) Workflow
-- -----------------------------
CREATE TABLE IF NOT EXISTS kms.workflow_template (
  uid INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  code TEXT NOT NULL UNIQUE,
  name TEXT NOT NULL,
  steps JSONB NOT NULL,        -- [{name:'Internal Draft'}, {name:'Client Review'}, ...]
  active BOOLEAN NOT NULL DEFAULT TRUE,
  created_by_user_fk INT REFERENCES kms.usertb(uid) ON DELETE SET NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE TABLE IF NOT EXISTS kms.workflow_instance (
  uid INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  document_fk INT NOT NULL REFERENCES kms.document(uid) ON DELETE CASCADE,
  workflow_template_fk INT REFERENCES kms.workflow_template(uid) ON DELETE SET NULL,
  name TEXT NOT NULL,
  current_step INT NOT NULL DEFAULT 0,
  state TEXT NOT NULL DEFAULT 'active' CHECK (state IN ('active','completed','cancelled','blocked')),
  created_by_user_fk INT REFERENCES kms.usertb(uid) ON DELETE SET NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);
CREATE UNIQUE INDEX IF NOT EXISTS uq_wf_doc_active ON kms.workflow_instance(document_fk) WHERE state='active';

CREATE TABLE IF NOT EXISTS kms.workflow_step (
  uid INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  workflow_instance_fk INT NOT NULL REFERENCES kms.workflow_instance(uid) ON DELETE CASCADE,
  step_index INT NOT NULL,
  name TEXT NOT NULL,
  state TEXT NOT NULL DEFAULT 'todo' CHECK (state IN ('todo','in_progress','done','blocked')),
  assigned_user_fk INT REFERENCES kms.usertb(uid) ON DELETE SET NULL,
  assigned_group_fk INT REFERENCES kms.grouptb(uid) ON DELETE SET NULL,
  started_at TIMESTAMPTZ,
  done_at TIMESTAMPTZ,
  UNIQUE (workflow_instance_fk, step_index)
);

-- -----------------------------
-- 6) Bundles (Regulator/Auditor packages)
-- -----------------------------
CREATE TABLE IF NOT EXISTS kms.bundle (
  uid INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  code TEXT NOT NULL UNIQUE,     -- e.g., 'CSSF-2025-Q4'
  name TEXT NOT NULL,
  purpose TEXT NOT NULL CHECK (purpose IN ('regulator','auditor','other')),
  created_by_user_fk INT REFERENCES kms.usertb(uid) ON DELETE SET NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE TABLE IF NOT EXISTS kms.bundle_item (
  uid INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  bundle_fk INT NOT NULL REFERENCES kms.bundle(uid) ON DELETE CASCADE,
  document_fk INT NOT NULL REFERENCES kms.document(uid) ON DELETE CASCADE,
  document_version_fk INT NOT NULL REFERENCES kms.document_version(uid) ON DELETE RESTRICT,
  fingerprint_sha256 TEXT NOT NULL,
  serialized_uri TEXT,
  added_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  UNIQUE (bundle_fk, document_fk)
);

-- -----------------------------
-- 7) Policies (export/import/AI/residency)
-- -----------------------------
CREATE TABLE IF NOT EXISTS kms.policy_rule (
  uid INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  domain_fk INT REFERENCES kms.domaintb(uid) ON DELETE CASCADE,
  tier_fk INT REFERENCES kms.tier(uid) ON DELETE CASCADE,
  status_fk INT REFERENCES kms.statustb(uid) ON DELETE CASCADE,
  residency TEXT,                          -- 'EU','UAE', etc.
  block_export BOOLEAN NOT NULL DEFAULT FALSE,
  block_import BOOLEAN NOT NULL DEFAULT FALSE,
  view_only BOOLEAN NOT NULL DEFAULT FALSE,
  ai_default BOOLEAN,                       -- NULL = inherit
  priority INT NOT NULL DEFAULT 0,
  created_by_user_fk INT REFERENCES kms.usertb(uid) ON DELETE SET NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);
CREATE INDEX IF NOT EXISTS idx_policy_scope ON kms.policy_rule(domain_fk, tier_fk, status_fk, residency, priority DESC);

-- -----------------------------
-- 8) Audit / Security events
-- -----------------------------
CREATE TABLE IF NOT EXISTS kms.audit_event (
  uid INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  ts TIMESTAMPTZ NOT NULL DEFAULT now(),
  actor_user_fk INT REFERENCES kms.usertb(uid) ON DELETE SET NULL,
  actor_org_fk INT REFERENCES kms.organization(uid) ON DELETE SET NULL,
  action_code TEXT NOT NULL,            -- 'ui.view:sankey','doc.create','acl.add','export.md'
  document_fk INT REFERENCES kms.document(uid) ON DELETE SET NULL,
  target_type TEXT,                     -- 'document','comment','workflow','bundle','policy','attachment','auth'
  target_uid INT,                      -- polymorphic target id
  ip INET,
  user_agent TEXT,
  meta JSONB
);
CREATE INDEX IF NOT EXISTS idx_audit_doc ON kms.audit_event(document_fk, ts DESC);
CREATE INDEX IF NOT EXISTS idx_audit_actor ON kms.audit_event(actor_user_fk, ts DESC);

-- -----------------------------
-- 9) Convenience views for UI
-- -----------------------------
CREATE OR REPLACE VIEW kms.v_counts_domain_status AS
SELECT d.domain_fk, d.status_fk, COUNT(*) AS cnt
FROM kms.document d
WHERE d.deleted_at IS NULL
GROUP BY d.domain_fk, d.status_fk;

CREATE OR REPLACE VIEW kms.v_counts_tier AS
SELECT d.tier_fk, COUNT(*) AS cnt
FROM kms.document d
WHERE d.deleted_at IS NULL
GROUP BY d.tier_fk;

CREATE OR REPLACE VIEW kms.v_constellation AS
SELECT d.uid AS document_uid,
       d.title,
       d.domain_fk,
       d.doc_type_fk,
       d.tier_fk,
       d.status_fk,
       s.order_index AS lifecycle_index
FROM kms.document d
JOIN kms.statustb s ON s.uid = d.status_fk
WHERE d.deleted_at IS NULL;

-- -----------------------------
-- 10) Permission check function (baseline)
-- -----------------------------
CREATE OR REPLACE FUNCTION kms.can_user(p_user INT, p_perm TEXT, p_document INT)
RETURNS BOOLEAN
LANGUAGE plpgsql
AS $$
DECLARE
  v_dom INT; v_tier INT; v_dom_code TEXT; v_tier_code TEXT;
  v_ok BOOLEAN := FALSE;
BEGIN
  -- Resolve document context (domain/tier codes are used for scoped checks)
  SELECT d.domain_fk, d.tier_fk, dm.code, tr.code
  INTO v_dom, v_tier, v_dom_code, v_tier_code
  FROM kms.document d
  JOIN kms.domaintb dm ON dm.uid = d.domain_fk
  JOIN kms.tier     tr ON tr.uid  = d.tier_fk
  WHERE d.uid = p_document;
  IF NOT FOUND THEN
    RETURN FALSE;
  END IF;

  -- Effective role-bindings for the user (direct, via group, via org)
  WITH u_org AS (
    SELECT org_fk FROM kms.usertb WHERE uid = p_user
  ), u_groups AS (
    SELECT group_fk FROM kms.group_member WHERE user_fk = p_user
  ), eff_bind AS (
    SELECT rb.*
    FROM kms.role_binding rb
    WHERE
      (rb.principal = 'user'  AND rb.user_fk  = p_user) OR
      (rb.principal = 'group' AND rb.group_fk IN (SELECT group_fk FROM u_groups)) OR
      (rb.principal = 'org'   AND rb.org_fk   = (SELECT org_fk FROM u_org))
  )
  -- 1) Document ACL direct grants (including principal='role' using eff_bind)
  SELECT TRUE INTO v_ok
  FROM kms.document_acl a
  WHERE a.document_fk = p_document
    AND p_perm = ANY(a.rights)
    AND (
      (a.principal='user'  AND a.user_fk  = p_user) OR
      (a.principal='group' AND a.group_fk IN (SELECT group_fk FROM kms.group_member WHERE user_fk = p_user)) OR
      (a.principal='org'   AND a.org_fk   = (SELECT org_fk FROM kms.usertb WHERE uid = p_user)) OR
      (a.principal='role'  AND a.role_fk  IN (SELECT role_fk FROM eff_bind
                                              WHERE eff_bind.scope = 'tenant'
                                                 OR (eff_bind.scope='domaintb' AND eff_bind.scope_value = v_dom_code)
                                                 OR (eff_bind.scope='resource' AND eff_bind.scope_value::int = p_document)))
    )
    AND (
      a.scope IS NULL
      OR (a.scope='domaintb' AND a.scope_value = v_dom_code)
      OR (a.scope='resource' AND a.scope_value::int = p_document)
    )
  LIMIT 1;

  IF v_ok THEN
    RETURN TRUE;
  END IF;

  -- 2) Role-based grants (role -> permission), using eff_bind + scope
  WITH eff_bind AS (
    SELECT rb.*
    FROM kms.role_binding rb
    WHERE
      (rb.principal='user'  AND rb.user_fk  = p_user) OR
      (rb.principal='group' AND rb.group_fk IN (SELECT group_fk FROM kms.group_member WHERE user_fk = p_user)) OR
      (rb.principal='org'   AND rb.org_fk   = (SELECT org_fk FROM kms.usertb WHERE uid = p_user))
  )
  SELECT TRUE INTO v_ok
  FROM eff_bind rb
  JOIN kms.role_permission rp ON rp.role_fk = rb.role_fk
  JOIN kms.permission      p  ON p.uid = rp.permission_fk AND p.code = p_perm
  WHERE
    rb.scope = 'tenant'
    OR (rb.scope='domaintb' AND rb.scope_value = v_dom_code)
    OR (rb.scope='resource' AND rb.scope_value::int = p_document)
  LIMIT 1;

  RETURN COALESCE(v_ok, FALSE);
END;
$$;

-- -----------------------------
-- 11) Indexing & triggers
-- -----------------------------
CREATE INDEX IF NOT EXISTS idx_doc_title_trgm ON kms.document USING GIN (title gin_trgm_ops);
CREATE INDEX IF NOT EXISTS idx_doc_status ON kms.document(status_fk);
CREATE INDEX IF NOT EXISTS idx_doc_domaintb ON kms.document(domain_fk);
CREATE INDEX IF NOT EXISTS idx_doc_tier ON kms.document(tier_fk);
CREATE INDEX IF NOT EXISTS idx_doc_due ON kms.document(due_date);

CREATE OR REPLACE FUNCTION kms.f_set_updated_at() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN NEW.updated_at = now(); RETURN NEW; END; $$;
DROP TRIGGER IF EXISTS trg_document_mtime ON kms.document;
CREATE TRIGGER trg_document_mtime BEFORE UPDATE ON kms.document FOR EACH ROW EXECUTE FUNCTION kms.f_set_updated_at();

-- -----------------------------
-- 12) RLS scaffolding (optional)
-- -----------------------------
-- ALTER TABLE document ENABLE ROW LEVEL SECURITY;
-- CREATE POLICY doc_policy_read ON document FOR SELECT USING (can_user(current_setting('app.user_uid', true)::int, 'view', uid));
-- CREATE POLICY doc_policy_write ON document FOR UPDATE USING (can_user(current_setting('app.user_uid', true)::int, 'edit', uid));

-- -----------------------------
-- 13) Seed catalogs
-- -----------------------------
INSERT INTO kms.domaintb(code,name) VALUES
  ('Governance','Governance'),('Regulatory','Regulatory')
  ,('Tax','Tax'),('Compliance','Compliance')
  ,('Structuring','Structuring'),('Capital Markets','Capital Markets')
ON CONFLICT (code) DO NOTHING;

INSERT INTO kms.doc_type(code,name) VALUES
  ('Memo','Memo'),('Policy','Policy')
  ,('Template','Template'),('Agreement','Agreement')
  ,('Register','Register'),('Procedure','Procedure')
ON CONFLICT (code) DO NOTHING;

INSERT INTO kms.tier(code,name) VALUES
  ('Public','Public'), ('Internal','Internal')
  ,('Restricted','Restricted'), ('Secret','Secret')
ON CONFLICT (code) DO NOTHING;

INSERT INTO kms.statustb(code,name,order_index) VALUES
    ('Idea','Idea',0)
  , ('Draft','Draft',1)
  , ('Review','Review',2)
  , ('Counsel','Counsel',3)
  , ('Approved','Approved',4)
  , ('Final','Final',5)
ON CONFLICT (code) DO NOTHING;


-- ============================================================
-- HBDoc v2 block storage
-- Replace the current blocks_bd / block_asset / helper section
-- with this version.
-- ============================================================

CREATE SEQUENCE IF NOT EXISTS kms.document_version_seq
  START WITH 1 INCREMENT BY 1;

-- ------------------------------------------------------------------
-- Base block kind catalog
--
-- We store the normalized base kind in kind_code.
-- For parameterized constructors:
--   NoteKB NoteKind  -> kind_code='note',   kind_arg='footnote'|'endnote'
--   CustomKB Text    -> kind_code='custom', kind_arg='<tag>'
-- ------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS kms.block_kind (
  code TEXT PRIMARY KEY
);

INSERT INTO kms.block_kind(code)
VALUES
    ('container')
  , ('heading')
  , ('paragraph')
  , ('list')
  , ('list_item')
  , ('quote')
  , ('code')
  , ('table')
  , ('table_row')
  , ('table_cell')
  , ('figure')
  , ('image')
  , ('rule')
  , ('note')
  , ('conversation')
  , ('message')
  , ('custom')
ON CONFLICT DO NOTHING;

-- ------------------------------------------------------------------
-- Block revisions
--
-- uid        = DB row identity for this stored revision row
-- eid        = stable logical block identity across revisions
--
-- content    = visible direct text for blocks that use it
-- sem        = JSONB encoding of SemanticsBlk
-- attrs      = JSONB encoding of AttributesBlk
-- provenance = JSONB encoding of ProvenanceBlk
--
-- valid_from_seq / valid_to_seq keep the existing time-travel shape.
-- ------------------------------------------------------------------
DROP TABLE IF EXISTS kms.blocks_bd CASCADE;
CREATE TABLE kms.blocks_bd (
    uid BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY
  , eid UUID NOT NULL DEFAULT public.uuid_generate_v4()
  , doc_fk INT NOT NULL REFERENCES kms.document(uid) ON DELETE CASCADE
  , parent_fk BIGINT NULL REFERENCES kms.blocks_bd(uid) ON DELETE CASCADE

  , kind_code TEXT NOT NULL REFERENCES kms.block_kind(code)
  , kind_arg TEXT NULL

  , content TEXT NULL
  , sem JSONB NULL
  , attrs JSONB NOT NULL DEFAULT '{}'::jsonb
  , provenance JSONB NULL

  , seq_pos NUMERIC(38,10) NOT NULL

  , valid_from_seq BIGINT NOT NULL DEFAULT nextval('kms.document_version_seq')
  , valid_to_seq BIGINT NULL

  , created_at TIMESTAMPTZ NOT NULL DEFAULT now()
  , created_by_user_fk INT NULL REFERENCES kms.usertb(uid)
);

CREATE INDEX IF NOT EXISTS idx_blocks_doc_parent_live_order
  ON kms.blocks_bd(doc_fk, parent_fk, seq_pos)
  WHERE valid_to_seq IS NULL;

CREATE INDEX IF NOT EXISTS idx_blocks_doc_seq_window
  ON kms.blocks_bd(doc_fk, valid_from_seq, valid_to_seq);

CREATE INDEX IF NOT EXISTS idx_blocks_eid
  ON kms.blocks_bd(eid);

CREATE INDEX IF NOT EXISTS idx_blocks_kind_live
  ON kms.blocks_bd(doc_fk, kind_code, kind_arg)
  WHERE valid_to_seq IS NULL;

CREATE INDEX IF NOT EXISTS idx_blocks_sem_gin
  ON kms.blocks_bd
  USING GIN (sem);

CREATE INDEX IF NOT EXISTS idx_blocks_attrs_gin
  ON kms.blocks_bd
  USING GIN (attrs);

CREATE INDEX IF NOT EXISTS idx_blocks_provenance_gin
  ON kms.blocks_bd
  USING GIN (provenance);

ALTER TABLE kms.blocks_bd
  ADD CONSTRAINT chk_blocks_note_arg
  CHECK (
    kind_code <> 'note'
    OR kind_arg IN ('footnote', 'endnote')
  );

ALTER TABLE kms.blocks_bd
  ADD CONSTRAINT chk_blocks_custom_arg
  CHECK (
    kind_code <> 'custom'
    OR kind_arg IS NOT NULL
  );

-- ------------------------------------------------------------------
-- Optional version labels
-- ------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS kms.version_labels (
    document_fk INT NOT NULL REFERENCES kms.document(uid)
  , label TEXT NOT NULL
  , at_seq BIGINT NOT NULL
  , at_time TIMESTAMPTZ NOT NULL DEFAULT now()
  , PRIMARY KEY(document_fk, label)
);

-- ------------------------------------------------------------------
-- Change-event audit
-- ------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS kms.change_events (
    uid BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY
  , document_fk INT NOT NULL REFERENCES kms.document(uid)
  , block_id BIGINT NOT NULL
  , event_type TEXT NOT NULL
  , event_data JSONB NOT NULL
  , occurred_at TIMESTAMPTZ NOT NULL DEFAULT now()
  , undone BOOLEAN NOT NULL DEFAULT FALSE
  , actor_fk INT NULL REFERENCES kms.usertb(uid)
);

CREATE INDEX IF NOT EXISTS idx_change_events_doc_time_live
  ON kms.change_events(document_fk, occurred_at DESC)
  WHERE NOT undone;

-- ------------------------------------------------------------------
-- Block-assets link
--
-- Composite PK lets one block reference multiple attachments if needed.
-- ------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS kms.block_asset (
    block_fk BIGINT NOT NULL REFERENCES kms.blocks_bd(uid) ON DELETE CASCADE
  , attachment_fk INT NOT NULL REFERENCES kms.attachment(uid) ON DELETE CASCADE
  , PRIMARY KEY (block_fk, attachment_fk)
);

-- ------------------------------------------------------------------
-- Root block helper
--
-- Root is now a true HBDoc container block, not an old “document” block.
-- ------------------------------------------------------------------
CREATE OR REPLACE FUNCTION kms.ensure_doc_root(
  p_doc INT,
  p_actor INT
) RETURNS BIGINT
LANGUAGE plpgsql AS $$
DECLARE
  v_root BIGINT;
BEGIN
  SELECT uid
    INTO v_root
    FROM kms.blocks_bd
   WHERE doc_fk = p_doc
     AND parent_fk IS NULL
     AND valid_to_seq IS NULL
   ORDER BY seq_pos ASC
   LIMIT 1;

  IF v_root IS NULL THEN
    PERFORM nextval('kms.document_version_seq');

    INSERT INTO kms.blocks_bd
      (doc_fk, parent_fk, kind_code, kind_arg, content, sem, attrs, provenance, seq_pos, valid_from_seq, created_by_user_fk)
    VALUES
      (p_doc, NULL, 'container', NULL, NULL, NULL, '{}'::jsonb, NULL, 1000000, currval('kms.document_version_seq'), p_actor)
    RETURNING uid INTO v_root;

    INSERT INTO kms.change_events(document_fk, block_id, event_type, event_data, actor_fk)
    VALUES
      ( p_doc
      , v_root
      , 'insert'
      , jsonb_build_object(
          'sequence', currval('kms.document_version_seq'),
          'kind_code', 'container'
        )
      , p_actor
      );
  END IF;

  RETURN v_root;
END $$;

-- ------------------------------------------------------------------
-- Append helper used by importer / serializer
--
-- This is the key shape change for HBDoc v2:
-- kind_code + kind_arg + content + sem + attrs + provenance
-- ------------------------------------------------------------------
CREATE OR REPLACE FUNCTION kms.append_child_import(
  p_doc INT,
  p_parent BIGINT,
  p_kind_code TEXT,
  p_kind_arg TEXT,
  p_content TEXT,
  p_sem JSONB,
  p_attrs JSONB,
  p_provenance JSONB,
  p_actor INT
) RETURNS BIGINT
LANGUAGE plpgsql AS $$
DECLARE
  v_pos NUMERIC;
  v_seq BIGINT;
  v_uid BIGINT;
BEGIN
  SELECT COALESCE(MAX(seq_pos), 0) + 1000
    INTO v_pos
    FROM kms.blocks_bd
   WHERE doc_fk = p_doc
     AND (
       (parent_fk IS NULL AND p_parent IS NULL)
       OR parent_fk = p_parent
     )
     AND valid_to_seq IS NULL;

  v_seq := nextval('kms.document_version_seq');

  INSERT INTO kms.blocks_bd
    (doc_fk, parent_fk, kind_code, kind_arg, content, sem, attrs, provenance, seq_pos, valid_from_seq, created_by_user_fk)
  VALUES
    ( p_doc
    , p_parent
    , p_kind_code
    , p_kind_arg
    , p_content
    , p_sem
    , COALESCE(p_attrs, '{}'::jsonb)
    , p_provenance
    , v_pos
    , v_seq
    , p_actor
    )
  RETURNING uid INTO v_uid;

  INSERT INTO kms.change_events(document_fk, block_id, event_type, event_data, actor_fk)
  VALUES
    ( p_doc
    , v_uid
    , 'insert'
    , jsonb_build_object(
        'sequence', v_seq,
        'parent_fk', p_parent,
        'kind_code', p_kind_code,
        'kind_arg', p_kind_arg,
        'seq_pos', v_pos
      )
    , p_actor
    );

  RETURN v_uid;
END $$;

-- ------------------------------------------------------------------
-- Link block to attachment
-- ------------------------------------------------------------------
CREATE OR REPLACE FUNCTION kms.link_block_attachment(
  p_block BIGINT,
  p_attachment INT,
  p_actor INT
) RETURNS VOID
LANGUAGE plpgsql AS $$
BEGIN
  INSERT INTO kms.block_asset(block_fk, attachment_fk)
  VALUES (p_block, p_attachment)
  ON CONFLICT DO NOTHING;

  INSERT INTO kms.change_events(document_fk, block_id, event_type, event_data, actor_fk)
  SELECT
      b.doc_fk
    , p_block
    , 'link_asset'
    , jsonb_build_object(
        'seq_pos', b.seq_pos,
        'attachment_uid', p_attachment
      )
    , p_actor
  FROM kms.blocks_bd b
  WHERE b.uid = p_block;
END $$;

-- ------------------------------------------------------------------
-- Optional helper
-- ------------------------------------------------------------------
CREATE OR REPLACE FUNCTION kms.last_child_uid(
  p_doc INT,
  p_parent BIGINT
) RETURNS BIGINT
LANGUAGE sql AS $$
  SELECT uid
    FROM kms.blocks_bd
   WHERE doc_fk = p_doc
     AND parent_fk IS NOT DISTINCT FROM p_parent
     AND valid_to_seq IS NULL
   ORDER BY seq_pos DESC
   LIMIT 1;
$$;

-- ------------------------------------------------------------------
-- Read helpers at sequence
--
-- These now expose the full HBDoc block payload needed for deserialization.
-- ------------------------------------------------------------------
CREATE OR REPLACE FUNCTION kms.get_blocks_dfs_at_seq(
  p_document_id INT,
  p_as_of_seq BIGINT,
  p_max_depth INT DEFAULT NULL,
  p_offset BIGINT DEFAULT 0,
  p_limit BIGINT DEFAULT NULL
) RETURNS TABLE(
    depth INT
  , block_id BIGINT
  , parent_block_id BIGINT
  , kind_code TEXT
  , kind_arg TEXT
  , content TEXT
  , sem JSONB
  , attrs JSONB
  , provenance JSONB
  , seq_pos NUMERIC(38,10)
  , has_more BOOLEAN
)
LANGUAGE sql AS $$
WITH RECURSIVE tree AS (
  SELECT
      1 AS depth
    , b.uid AS block_id
    , b.parent_fk AS parent_block_id
    , b.kind_code
    , b.kind_arg
    , b.content
    , b.sem
    , b.attrs
    , b.provenance
    , b.seq_pos
    , (
        p_max_depth IS NOT NULL
        AND 1 = p_max_depth
        AND EXISTS (
          SELECT 1
            FROM kms.blocks_bd c
           WHERE c.parent_fk = b.uid
             AND c.valid_from_seq <= p_as_of_seq
             AND (c.valid_to_seq IS NULL OR c.valid_to_seq > p_as_of_seq)
        )
      ) AS has_more
    , ARRAY[b.seq_pos] AS path
  FROM kms.blocks_bd b
  WHERE b.doc_fk = p_document_id
    AND b.parent_fk IS NULL
    AND b.valid_from_seq <= p_as_of_seq
    AND (b.valid_to_seq IS NULL OR b.valid_to_seq > p_as_of_seq)

  UNION ALL

  SELECT
      t.depth + 1
    , c.uid
    , c.parent_fk
    , c.kind_code
    , c.kind_arg
    , c.content
    , c.sem
    , c.attrs
    , c.provenance
    , c.seq_pos
    , (
        p_max_depth IS NOT NULL
        AND t.depth + 1 = p_max_depth
        AND EXISTS (
          SELECT 1
            FROM kms.blocks_bd x
           WHERE x.parent_fk = c.uid
             AND x.valid_from_seq <= p_as_of_seq
             AND (x.valid_to_seq IS NULL OR x.valid_to_seq > p_as_of_seq)
        )
      ) AS has_more
    , (t.path || c.seq_pos)::numeric(38,10)[] AS path
  FROM tree t
  JOIN kms.blocks_bd c
    ON c.parent_fk = t.block_id
   AND c.doc_fk = p_document_id
   AND c.valid_from_seq <= p_as_of_seq
   AND (c.valid_to_seq IS NULL OR c.valid_to_seq > p_as_of_seq)
  WHERE p_max_depth IS NULL OR t.depth < p_max_depth
)
SELECT
    depth
  , block_id
  , parent_block_id
  , kind_code
  , kind_arg
  , content
  , sem
  , attrs
  , provenance
  , seq_pos
  , has_more
FROM tree
ORDER BY path
OFFSET GREATEST(p_offset, 0)
LIMIT CASE WHEN p_limit IS NULL OR p_limit < 0 THEN NULL ELSE p_limit END;
$$;

CREATE OR REPLACE FUNCTION kms.document_blocks_as_of_seq(
  p_document_id INT,
  p_as_of_seq BIGINT
) RETURNS TABLE (
    block_id BIGINT
  , document_id INT
  , parent_block_id BIGINT
  , seq_pos NUMERIC(38,10)
  , kind_code TEXT
  , kind_arg TEXT
  , content TEXT
  , sem JSONB
  , attrs JSONB
  , provenance JSONB
)
LANGUAGE sql STABLE AS $$
  SELECT
      uid
    , doc_fk
    , parent_fk
    , seq_pos
    , kind_code
    , kind_arg
    , content
    , sem
    , attrs
    , provenance
    FROM kms.blocks_bd
   WHERE doc_fk = p_document_id
     AND valid_from_seq <= p_as_of_seq
     AND (valid_to_seq IS NULL OR valid_to_seq > p_as_of_seq)
   ORDER BY parent_fk NULLS FIRST, seq_pos;
$$;


-- ------------------------------------------------------------------
-- Resequence the live children under one parent.
--
-- This is intentionally an in-place maintenance operation over live rows.
-- It keeps the current rows and simply normalizes seq_pos to 1000-step gaps.
-- ------------------------------------------------------------------
CREATE OR REPLACE FUNCTION kms.resequence_blocks(
  p_doc INT,
  p_parent BIGINT,
  p_actor INT DEFAULT NULL
) RETURNS VOID
LANGUAGE plpgsql AS $$
BEGIN
  WITH ordered AS (
    SELECT
        b.uid
      , (row_number() OVER (ORDER BY b.seq_pos, b.uid) * 1000)::numeric(38,10) AS new_seq_pos
    FROM kms.blocks_bd b
    WHERE b.doc_fk = p_doc
      AND b.parent_fk IS NOT DISTINCT FROM p_parent
      AND b.valid_to_seq IS NULL
  ),
  updated AS (
    UPDATE kms.blocks_bd b
       SET seq_pos = o.new_seq_pos
      FROM ordered o
     WHERE b.uid = o.uid
     RETURNING b.uid, b.seq_pos
  )
  INSERT INTO kms.change_events(document_fk, block_id, event_type, event_data, actor_fk)
  SELECT
      p_doc
    , u.uid
    , 'resequence'
    , jsonb_build_object(
        'parent_fk', p_parent,
        'new_seq_pos', u.seq_pos
      )
    , p_actor
  FROM updated u
  WHERE p_actor IS NOT NULL;
END $$;

-- TODO: use this for the insert_block_* functions to recover simultaneously both id/eid.
CREATE TYPE kms.block_insert_result AS (
  block_id int,
  block_eid uuid
);

-- ------------------------------------------------------------------
-- Insert a new live sibling immediately after p_anchor_block.
--
-- The new row uses the full HBDoc v2 block payload.
-- ------------------------------------------------------------------
CREATE OR REPLACE FUNCTION kms.insert_block_after(
  p_anchor_block BIGINT,
  p_kind_code TEXT,
  p_kind_arg TEXT,
  p_content TEXT,
  p_sem JSONB,
  p_attrs JSONB,
  p_provenance JSONB,
  p_actor INT
) RETURNS BIGINT
LANGUAGE plpgsql AS $$
DECLARE
  v_doc INT;
  v_parent BIGINT;
  v_anchor_seq NUMERIC(38,10);
  v_next_seq NUMERIC(38,10);
  v_new_seq NUMERIC(38,10);
  v_new_uid BIGINT;
  v_seq BIGINT;
BEGIN
  SELECT b.doc_fk, b.parent_fk, b.seq_pos
    INTO v_doc, v_parent, v_anchor_seq
    FROM kms.blocks_bd b
   WHERE b.uid = p_anchor_block
     AND b.valid_to_seq IS NULL;

  IF NOT FOUND THEN
    RAISE EXCEPTION 'insert_block_after: anchor block % not found or not live', p_anchor_block;
  END IF;

  SELECT b.seq_pos
    INTO v_next_seq
    FROM kms.blocks_bd b
   WHERE b.doc_fk = v_doc
     AND b.parent_fk IS NOT DISTINCT FROM v_parent
     AND b.valid_to_seq IS NULL
     AND b.seq_pos > v_anchor_seq
   ORDER BY b.seq_pos ASC, b.uid ASC
   LIMIT 1;

  IF v_next_seq IS NULL THEN
    v_new_seq := (v_anchor_seq + 1000)::numeric(38,10);
  ELSE
    v_new_seq := ((v_anchor_seq + v_next_seq) / 2)::numeric(38,10);
  END IF;

  IF v_new_seq = v_anchor_seq OR (v_next_seq IS NOT NULL AND v_new_seq = v_next_seq) THEN
    PERFORM kms.resequence_blocks(v_doc, v_parent, p_actor);

    SELECT b.seq_pos
      INTO v_anchor_seq
      FROM kms.blocks_bd b
     WHERE b.uid = p_anchor_block
       AND b.valid_to_seq IS NULL;

    SELECT b.seq_pos
      INTO v_next_seq
      FROM kms.blocks_bd b
     WHERE b.doc_fk = v_doc
       AND b.parent_fk IS NOT DISTINCT FROM v_parent
       AND b.valid_to_seq IS NULL
       AND b.seq_pos > v_anchor_seq
     ORDER BY b.seq_pos ASC, b.uid ASC
     LIMIT 1;

    IF v_next_seq IS NULL THEN
      v_new_seq := (v_anchor_seq + 1000)::numeric(38,10);
    ELSE
      v_new_seq := ((v_anchor_seq + v_next_seq) / 2)::numeric(38,10);
    END IF;
  END IF;

  v_seq := nextval('kms.document_version_seq');

  INSERT INTO kms.blocks_bd
    (doc_fk, parent_fk, kind_code, kind_arg, content, sem, attrs, provenance, seq_pos, valid_from_seq, created_by_user_fk)
  VALUES
    ( v_doc
    , v_parent
    , p_kind_code
    , p_kind_arg
    , p_content
    , p_sem
    , COALESCE(p_attrs, '{}'::jsonb)
    , p_provenance
    , v_new_seq
    , v_seq
    , p_actor
    )
  RETURNING uid INTO v_new_uid;

  INSERT INTO kms.change_events(document_fk, block_id, event_type, event_data, actor_fk)
  VALUES
    ( v_doc
    , v_new_uid
    , 'insert_after'
    , jsonb_build_object(
        'anchor_block_uid', p_anchor_block,
        'parent_fk', v_parent,
        'seq_pos', v_new_seq,
        'kind_code', p_kind_code,
        'kind_arg', p_kind_arg,
        'sequence', v_seq
      )
    , p_actor
    );

  RETURN v_new_uid;
END $$;

-- ------------------------------------------------------------------
-- Insert a new live sibling immediately before p_anchor_block.
-- ------------------------------------------------------------------
CREATE OR REPLACE FUNCTION kms.insert_block_before(
  p_anchor_block BIGINT,
  p_kind_code TEXT,
  p_kind_arg TEXT,
  p_content TEXT,
  p_sem JSONB,
  p_attrs JSONB,
  p_provenance JSONB,
  p_actor INT
) RETURNS BIGINT
LANGUAGE plpgsql AS $$
DECLARE
  v_doc INT;
  v_parent BIGINT;
  v_anchor_seq NUMERIC(38,10);
  v_prev_seq NUMERIC(38,10);
  v_new_seq NUMERIC(38,10);
  v_new_uid BIGINT;
  v_seq BIGINT;
BEGIN
  SELECT b.doc_fk, b.parent_fk, b.seq_pos
    INTO v_doc, v_parent, v_anchor_seq
    FROM kms.blocks_bd b
   WHERE b.uid = p_anchor_block
     AND b.valid_to_seq IS NULL;

  IF NOT FOUND THEN
    RAISE EXCEPTION 'insert_block_before: anchor block % not found or not live', p_anchor_block;
  END IF;

  SELECT b.seq_pos
    INTO v_prev_seq
    FROM kms.blocks_bd b
   WHERE b.doc_fk = v_doc
     AND b.parent_fk IS NOT DISTINCT FROM v_parent
     AND b.valid_to_seq IS NULL
     AND b.seq_pos < v_anchor_seq
   ORDER BY b.seq_pos DESC, b.uid DESC
   LIMIT 1;

  IF v_prev_seq IS NULL THEN
    v_new_seq := (v_anchor_seq - 1000)::numeric(38,10);
  ELSE
    v_new_seq := ((v_prev_seq + v_anchor_seq) / 2)::numeric(38,10);
  END IF;

  IF v_new_seq = v_anchor_seq OR (v_prev_seq IS NOT NULL AND v_new_seq = v_prev_seq) THEN
    PERFORM kms.resequence_blocks(v_doc, v_parent, p_actor);

    SELECT b.seq_pos
      INTO v_anchor_seq
      FROM kms.blocks_bd b
     WHERE b.uid = p_anchor_block
       AND b.valid_to_seq IS NULL;

    SELECT b.seq_pos
      INTO v_prev_seq
      FROM kms.blocks_bd b
     WHERE b.doc_fk = v_doc
       AND b.parent_fk IS NOT DISTINCT FROM v_parent
       AND b.valid_to_seq IS NULL
       AND b.seq_pos < v_anchor_seq
     ORDER BY b.seq_pos DESC, b.uid DESC
     LIMIT 1;

    IF v_prev_seq IS NULL THEN
      v_new_seq := (v_anchor_seq - 1000)::numeric(38,10);
    ELSE
      v_new_seq := ((v_prev_seq + v_anchor_seq) / 2)::numeric(38,10);
    END IF;
  END IF;

  v_seq := nextval('kms.document_version_seq');

  INSERT INTO kms.blocks_bd
    (doc_fk, parent_fk, kind_code, kind_arg, content, sem, attrs, provenance, seq_pos, valid_from_seq, created_by_user_fk)
  VALUES
    ( v_doc
    , v_parent
    , p_kind_code
    , p_kind_arg
    , p_content
    , p_sem
    , COALESCE(p_attrs, '{}'::jsonb)
    , p_provenance
    , v_new_seq
    , v_seq
    , p_actor
    )
  RETURNING uid INTO v_new_uid;

  INSERT INTO kms.change_events(document_fk, block_id, event_type, event_data, actor_fk)
  VALUES
    ( v_doc
    , v_new_uid
    , 'insert_before'
    , jsonb_build_object(
        'anchor_block_uid', p_anchor_block,
        'parent_fk', v_parent,
        'seq_pos', v_new_seq,
        'kind_code', p_kind_code,
        'kind_arg', p_kind_arg,
        'sequence', v_seq
      )
    , p_actor
    );

  RETURN v_new_uid;
END $$;



-- ------------------------------------------------------------------
-- Move a live block to a new parent / position.
--
-- Practical note:
-- this updates the live row in place. That is the only tractable choice
-- with parent_fk pointing at block-row uid rather than a logical eid.
-- It preserves subtree integrity and records the move in change_events.
-- ------------------------------------------------------------------
CREATE OR REPLACE FUNCTION kms.move_block(
  p_block BIGINT,
  p_new_parent BIGINT,
  p_after_block BIGINT DEFAULT NULL,
  p_before_block BIGINT DEFAULT NULL,
  p_actor INT DEFAULT NULL
) RETURNS VOID
LANGUAGE plpgsql AS $$
DECLARE
  v_doc INT;
  v_old_parent BIGINT;
  v_old_seq NUMERIC(38,10);
  v_new_seq NUMERIC(38,10);
  v_prev_seq NUMERIC(38,10);
  v_next_seq NUMERIC(38,10);
BEGIN
  IF p_after_block IS NOT NULL AND p_before_block IS NOT NULL THEN
    RAISE EXCEPTION 'move_block: only one of p_after_block / p_before_block may be set';
  END IF;

  SELECT b.doc_fk, b.parent_fk, b.seq_pos
    INTO v_doc, v_old_parent, v_old_seq
    FROM kms.blocks_bd b
   WHERE b.uid = p_block
     AND b.valid_to_seq IS NULL;

  IF NOT FOUND THEN
    RAISE EXCEPTION 'move_block: block % not found or not live', p_block;
  END IF;

  IF v_old_parent IS NULL THEN
    RAISE EXCEPTION 'move_block: refusing to move document root block %', p_block;
  END IF;

  IF p_new_parent = p_block THEN
    RAISE EXCEPTION 'move_block: block % cannot become its own parent', p_block;
  END IF;

  IF EXISTS (
    WITH RECURSIVE subtree AS (
      SELECT uid
      FROM kms.blocks_bd
      WHERE uid = p_block
        AND valid_to_seq IS NULL
      UNION ALL
      SELECT c.uid
      FROM subtree s
      JOIN kms.blocks_bd c
        ON c.parent_fk = s.uid
       AND c.valid_to_seq IS NULL
    )
    SELECT 1
    FROM subtree
    WHERE uid = p_new_parent
  ) THEN
    RAISE EXCEPTION 'move_block: cannot move block % under its own descendant %', p_block, p_new_parent;
  END IF;

  IF p_after_block IS NOT NULL THEN
    SELECT b.seq_pos
      INTO v_prev_seq
      FROM kms.blocks_bd b
     WHERE b.uid = p_after_block
       AND b.valid_to_seq IS NULL
       AND b.doc_fk = v_doc
       AND b.parent_fk = p_new_parent;

    IF NOT FOUND THEN
      RAISE EXCEPTION 'move_block: after-block % not found under requested parent %', p_after_block, p_new_parent;
    END IF;

    SELECT b.seq_pos
      INTO v_next_seq
      FROM kms.blocks_bd b
     WHERE b.doc_fk = v_doc
       AND b.parent_fk = p_new_parent
       AND b.valid_to_seq IS NULL
       AND b.uid <> p_block
       AND b.seq_pos > v_prev_seq
     ORDER BY b.seq_pos ASC, b.uid ASC
     LIMIT 1;

    IF v_next_seq IS NULL THEN
      v_new_seq := (v_prev_seq + 1000)::numeric(38,10);
    ELSE
      v_new_seq := ((v_prev_seq + v_next_seq) / 2)::numeric(38,10);
    END IF;

    IF v_new_seq = v_prev_seq OR (v_next_seq IS NOT NULL AND v_new_seq = v_next_seq) THEN
      PERFORM kms.resequence_blocks(v_doc, p_new_parent, p_actor);

      SELECT b.seq_pos
        INTO v_prev_seq
        FROM kms.blocks_bd b
       WHERE b.uid = p_after_block
         AND b.valid_to_seq IS NULL;

      SELECT b.seq_pos
        INTO v_next_seq
        FROM kms.blocks_bd b
       WHERE b.doc_fk = v_doc
         AND b.parent_fk = p_new_parent
         AND b.valid_to_seq IS NULL
         AND b.uid <> p_block
         AND b.seq_pos > v_prev_seq
       ORDER BY b.seq_pos ASC, b.uid ASC
       LIMIT 1;

      IF v_next_seq IS NULL THEN
        v_new_seq := (v_prev_seq + 1000)::numeric(38,10);
      ELSE
        v_new_seq := ((v_prev_seq + v_next_seq) / 2)::numeric(38,10);
      END IF;
    END IF;

  ELSIF p_before_block IS NOT NULL THEN
    SELECT b.seq_pos
      INTO v_next_seq
      FROM kms.blocks_bd b
     WHERE b.uid = p_before_block
       AND b.valid_to_seq IS NULL
       AND b.doc_fk = v_doc
       AND b.parent_fk = p_new_parent;

    IF NOT FOUND THEN
      RAISE EXCEPTION 'move_block: before-block % not found under requested parent %', p_before_block, p_new_parent;
    END IF;

    SELECT b.seq_pos
      INTO v_prev_seq
      FROM kms.blocks_bd b
     WHERE b.doc_fk = v_doc
       AND b.parent_fk = p_new_parent
       AND b.valid_to_seq IS NULL
       AND b.uid <> p_block
       AND b.seq_pos < v_next_seq
     ORDER BY b.seq_pos DESC, b.uid DESC
     LIMIT 1;

    IF v_prev_seq IS NULL THEN
      v_new_seq := (v_next_seq - 1000)::numeric(38,10);
    ELSE
      v_new_seq := ((v_prev_seq + v_next_seq) / 2)::numeric(38,10);
    END IF;

    IF v_new_seq = v_next_seq OR (v_prev_seq IS NOT NULL AND v_new_seq = v_prev_seq) THEN
      PERFORM kms.resequence_blocks(v_doc, p_new_parent, p_actor);

      SELECT b.seq_pos
        INTO v_next_seq
        FROM kms.blocks_bd b
       WHERE b.uid = p_before_block
         AND b.valid_to_seq IS NULL;

      SELECT b.seq_pos
        INTO v_prev_seq
        FROM kms.blocks_bd b
       WHERE b.doc_fk = v_doc
         AND b.parent_fk = p_new_parent
         AND b.valid_to_seq IS NULL
         AND b.uid <> p_block
         AND b.seq_pos < v_next_seq
       ORDER BY b.seq_pos DESC, b.uid DESC
       LIMIT 1;

      IF v_prev_seq IS NULL THEN
        v_new_seq := (v_next_seq - 1000)::numeric(38,10);
      ELSE
        v_new_seq := ((v_prev_seq + v_next_seq) / 2)::numeric(38,10);
      END IF;
    END IF;

  ELSE
    SELECT COALESCE(MAX(b.seq_pos), 0) + 1000
      INTO v_new_seq
      FROM kms.blocks_bd b
     WHERE b.doc_fk = v_doc
       AND b.parent_fk = p_new_parent
       AND b.valid_to_seq IS NULL
       AND b.uid <> p_block;
  END IF;

  UPDATE kms.blocks_bd
     SET parent_fk = p_new_parent
       , seq_pos = v_new_seq
   WHERE uid = p_block
     AND valid_to_seq IS NULL;

  INSERT INTO kms.change_events(document_fk, block_id, event_type, event_data, actor_fk)
  VALUES
    ( v_doc
    , p_block
    , 'move'
    , jsonb_build_object(
        'old_parent_fk', v_old_parent,
        'new_parent_fk', p_new_parent,
        'old_seq_pos', v_old_seq,
        'new_seq_pos', v_new_seq,
        'after_block_uid', p_after_block,
        'before_block_uid', p_before_block
      )
    , p_actor
    );
END $$;


-- ------------------------------------------------------------------
-- Delete a live subtree by closing all rows in that subtree at one seq.
-- ------------------------------------------------------------------
CREATE OR REPLACE FUNCTION kms.delete_block(
  p_block BIGINT,
  p_actor INT DEFAULT NULL
) RETURNS VOID
LANGUAGE plpgsql AS $$
DECLARE
  v_doc INT;
  v_parent BIGINT;
  v_seq BIGINT;
  v_deleted_count BIGINT;
BEGIN
  SELECT b.doc_fk, b.parent_fk
    INTO v_doc, v_parent
    FROM kms.blocks_bd b
   WHERE b.uid = p_block
     AND b.valid_to_seq IS NULL;

  IF NOT FOUND THEN
    RAISE EXCEPTION 'delete_block: block % not found or not live', p_block;
  END IF;

  IF v_parent IS NULL THEN
    RAISE EXCEPTION 'delete_block: refusing to delete document root block %', p_block;
  END IF;

  v_seq := nextval('kms.document_version_seq');

  WITH RECURSIVE subtree AS (
    SELECT b.uid
    FROM kms.blocks_bd b
    WHERE b.uid = p_block
      AND b.valid_to_seq IS NULL

    UNION ALL

    SELECT c.uid
    FROM subtree s
    JOIN kms.blocks_bd c
      ON c.parent_fk = s.uid
     AND c.valid_to_seq IS NULL
  ),
  updated AS (
    UPDATE kms.blocks_bd b
       SET valid_to_seq = v_seq
      FROM subtree s
     WHERE b.uid = s.uid
       AND b.valid_to_seq IS NULL
     RETURNING b.uid
  )
  SELECT count(*) INTO v_deleted_count
  FROM updated;

  INSERT INTO kms.change_events(document_fk, block_id, event_type, event_data, actor_fk)
  VALUES
    ( v_doc
    , p_block
    , 'delete'
    , jsonb_build_object(
        'sequence', v_seq,
        'deleted_count', v_deleted_count
      )
    , p_actor
    );
END $$;



-- ------------------------------------------------------------------
-- Read a subtree depth-first at a given sequence.
--
-- This mirrors the document-wide DFS helper but starts from one block row.
-- ------------------------------------------------------------------
CREATE OR REPLACE FUNCTION kms.get_subtree_dfs_at_seq(
  p_block_id BIGINT,
  p_as_of_seq BIGINT,
  p_max_depth INT DEFAULT NULL,
  p_offset BIGINT DEFAULT 0,
  p_limit BIGINT DEFAULT NULL
) RETURNS TABLE(
    depth INT
  , block_id BIGINT
  , parent_block_id BIGINT
  , kind_code TEXT
  , kind_arg TEXT
  , content TEXT
  , sem JSONB
  , attrs JSONB
  , provenance JSONB
  , seq_pos NUMERIC(38,10)
  , has_more BOOLEAN
)
LANGUAGE sql AS $$
WITH RECURSIVE tree AS (
  SELECT
      1 AS depth
    , b.uid AS block_id
    , b.parent_fk AS parent_block_id
    , b.kind_code
    , b.kind_arg
    , b.content
    , b.sem
    , b.attrs
    , b.provenance
    , b.seq_pos
    , (
        p_max_depth IS NOT NULL
        AND 1 = p_max_depth
        AND EXISTS (
          SELECT 1
          FROM kms.blocks_bd c
          WHERE c.parent_fk = b.uid
            AND c.valid_from_seq <= p_as_of_seq
            AND (c.valid_to_seq IS NULL OR c.valid_to_seq > p_as_of_seq)
        )
      ) AS has_more
    , ARRAY[b.seq_pos] AS path
  FROM kms.blocks_bd b
  WHERE b.uid = p_block_id
    AND b.valid_from_seq <= p_as_of_seq
    AND (b.valid_to_seq IS NULL OR b.valid_to_seq > p_as_of_seq)

  UNION ALL

  SELECT
      t.depth + 1
    , c.uid
    , c.parent_fk
    , c.kind_code
    , c.kind_arg
    , c.content
    , c.sem
    , c.attrs
    , c.provenance
    , c.seq_pos
    , (
        p_max_depth IS NOT NULL
        AND t.depth + 1 = p_max_depth
        AND EXISTS (
          SELECT 1
          FROM kms.blocks_bd x
          WHERE x.parent_fk = c.uid
            AND x.valid_from_seq <= p_as_of_seq
            AND (x.valid_to_seq IS NULL OR x.valid_to_seq > p_as_of_seq)
        )
      ) AS has_more
    , (t.path || c.seq_pos)::numeric(38,10)[] AS path
  FROM tree t
  JOIN kms.blocks_bd c
    ON c.parent_fk = t.block_id
   AND c.valid_from_seq <= p_as_of_seq
   AND (c.valid_to_seq IS NULL OR c.valid_to_seq > p_as_of_seq)
  WHERE p_max_depth IS NULL OR t.depth < p_max_depth
)
SELECT
    depth
  , block_id
  , parent_block_id
  , kind_code
  , kind_arg
  , content
  , sem
  , attrs
  , provenance
  , seq_pos
  , has_more
FROM tree
ORDER BY path
OFFSET GREATEST(p_offset, 0)
LIMIT CASE WHEN p_limit IS NULL OR p_limit < 0 THEN NULL ELSE p_limit END;
$$;


--- HBDoc v1 recovery: Categorisation ---
create table if not exists kms.catalog (
  uid INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY
  , label character varying(250)
  , owner_fk integer not null references kms.usertb(uid)
);

create table if not exists kms.nodecat (
  uid INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY
  , arboid integer NOT NULL references kms.catalog(uid)
  , label character varying(250)
  , parentid int
  , assetid int
  , lastmod timestamp with time zone
  , umode character(4)
);

-- --------------------------------------------------
-- 8. Version labeling
-- --------------------------------------------------
CREATE OR REPLACE FUNCTION kms.stamp_version_label(p_document_id INT,p_label TEXT) RETURNS VOID LANGUAGE plpgsql AS $$
DECLARE v_seq BIGINT := currval('kms.document_version_seq');
BEGIN
  INSERT INTO kms.version_labels(document_fk,label,at_seq,at_time)
    VALUES(p_document_id,p_label,v_seq,now());
END;
$$;
