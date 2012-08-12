-----------------------------------------------------------------------------------------------------
-- ADMINISTRATORS -----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------
CREATE TABLE administrators (
  id                    serial primary key,
  created               timestamp                                NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated               timestamp                                NOT NULL DEFAULT CURRENT_TIMESTAMP,
  email                 varchar(255)                             NOT NULL UNIQUE,
  password              varchar(255)                             NOT NULL
);
CREATE INDEX administrators_email_index ON administrators USING hash (email);   
-----------------------------------------------------------------------------------------------------
-- /ADMINISTRATORS ----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------


-----------------------------------------------------------------------------------------------------
-- DEVELOPERS ---------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------
CREATE TYPE account_status_type AS ENUM ('ok', 'overdue', 'suspended');
CREATE TABLE developers (
  id                    serial primary key,
  created               timestamp                                NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated               timestamp                                NOT NULL DEFAULT CURRENT_TIMESTAMP,
  email                 varchar(255)                             NOT NULL UNIQUE,
  password              varchar(255)                             NOT NULL,
  customer_id           varchar(255)                             NOT NULL UNIQUE,
  account_status        account_status_type                      NOT NULL DEFAULT 'ok'
);
CREATE INDEX developers_email_index ON developers USING hash (email);   
CREATE INDEX developers_account_status_index ON developers USING hash (account_status);   
-----------------------------------------------------------------------------------------------------
-- /DEVELOPERS --------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------


-----------------------------------------------------------------------------------------------------
-- APPLICATIONS -------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------
CREATE TABLE applications (
  id                    serial primary key,
  created               timestamp                                NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated               timestamp                                NOT NULL DEFAULT CURRENT_TIMESTAMP,
  name                  varchar(255)                             NOT NULL,
  usage                 bigint                                   NOT NULL DEFAULT 0,
  developer_id          integer REFERENCES developers (id)       NOT NULL,
  UNIQUE (name, developer_id)
);
CREATE INDEX applications_developer_id_index ON applications USING hash (developer_id);
-----------------------------------------------------------------------------------------------------
-- /APPLICATIONS ------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------


-----------------------------------------------------------------------------------------------------
-- USERS --------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------
CREATE TABLE users (
  id                    serial primary key,
  created               timestamp                                NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated               timestamp                                NOT NULL DEFAULT CURRENT_TIMESTAMP,
  email                 varchar(255)                             NOT NULL,
  password              varchar(255)                             NOT NULL,
  application_id        integer REFERENCES applications (id)     NOT NULL,
  UNIQUE (email, application_id)
);
CREATE INDEX users_application_id_index ON users USING hash (application_id);
CREATE INDEX users_email_index ON users USING hash (email);
-----------------------------------------------------------------------------------------------------
-- /USERS -------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------


-----------------------------------------------------------------------------------------------------
-- DEVELOPER ACCESS KEYS ----------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------
CREATE TABLE developer_access_grants (
  id                    serial primary key,
  created               timestamp                                NOT NULL DEFAULT CURRENT_TIMESTAMP,
  name                  varchar(255)                             NOT NULL,
  key                   varchar(255)                             NOT NULL UNIQUE,
  developer_id          integer REFERENCES developers (id)       NOT NULL,
  UNIQUE (name, developer_id)
);
CREATE INDEX developer_access_grants_dev_index ON developer_access_grants USING hash (developer_id);
CREATE INDEX developer_access_grants_key_index ON developer_access_grants USING hash (key);
-----------------------------------------------------------------------------------------------------
-- /DEVELOPER ACCESS KEYS ---------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------


-----------------------------------------------------------------------------------------------------
-- APPLICATION ACCESS KEYS --------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------
CREATE TABLE application_access_grants (
  id                    serial primary key,
  created               timestamp                                NOT NULL DEFAULT CURRENT_TIMESTAMP,
  name                  varchar(255)                             NOT NULL,
  key                   varchar(255)                             NOT NULL UNIQUE,
  application_id        integer REFERENCES applications (id)     NOT NULL,
  UNIQUE (name, application_id)
);
CREATE INDEX app_access_grants_app_index ON application_access_grants USING hash (application_id);
CREATE INDEX app_access_grants_key_index ON application_access_grants USING hash (key);
-----------------------------------------------------------------------------------------------------
-- /APPLICATION ACCESS KEYS -------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------


-----------------------------------------------------------------------------------------------------
-- USAGE CONSTRAINTS --------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------
CREATE TYPE service_type AS ENUM ('primary', 'pubsub', 'voice', 'hooks');
CREATE TABLE usage_constraints (
  id                    serial primary key,
  created               timestamp                                NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated               timestamp                                NOT NULL DEFAULT CURRENT_TIMESTAMP,
  service               service_type                             NOT NULL,
  level                 int                                      NOT NULL,
  capacity              bigint                                   NOT NULL,
  description           text                                     NOT NULL,
  UNIQUE (service, capacity),
  UNIQUE (service, level)
);
-----------------------------------------------------------------------------------------------------
-- /USAGE CONSTRAINTS -------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------


-----------------------------------------------------------------------------------------------------
-- DEVELOPER USAGE CONSTRAINTS ----------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------
CREATE TABLE developer_usage_constraints (
  id                    serial primary key,
  created               timestamp                                 NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated               timestamp                                 NOT NULL DEFAULT CURRENT_TIMESTAMP,
  developer_id          integer REFERENCES developers (id)        NOT NULL,
  usage_constraint_id   integer REFERENCES usage_constraints (id) NOT NULL
);
CREATE INDEX duc_developer_id ON developer_usage_constraints USING hash (developer_id);
CREATE INDEX duc_uc_id ON developer_usage_constraints USING hash (usage_constraint_id);
-----------------------------------------------------------------------------------------------------
-- /DEVELOPER USAGE CONSTRAINTS ---------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------


-----------------------------------------------------------------------------------------------------
-- LOGGED ACTIONS -----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------
CREATE TYPE actor_type AS ENUM ('developer', 'administrator');
CREATE TABLE logged_actions (
  id                    serial primary key,
  created               timestamp                                NOT NULL DEFAULT CURRENT_TIMESTAMP,
  actor                 actor_type                               NOT NULL,
  actor_id              integer                                  NOT NULL,
  description           varchar(255)                             NOT NULL
);
CREATE INDEX logged_actions_actor ON logged_actions USING hash (actor);
CREATE INDEX logged_actions_actor_id ON logged_actions USING hash (actor_id);
-----------------------------------------------------------------------------------------------------
-- /LOGGED ACTIONS ----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------
