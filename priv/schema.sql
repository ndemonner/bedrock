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
  appid                 varchar(255)                             NOT NULL UNIQUE,
  developer_id          integer REFERENCES developers (id)       ON DELETE CASCADE NOT NULL,
  UNIQUE (name, developer_id)
);
CREATE INDEX applications_developer_id_index ON applications USING hash (developer_id);
CREATE INDEX applications_appid_index ON applications USING hash (appid);
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
  application_id        integer REFERENCES applications (id)     ON DELETE CASCADE NOT NULL,
  UNIQUE (email, application_id)
);
CREATE INDEX users_application_id_index ON users USING hash (application_id);
CREATE INDEX users_email_index ON users USING hash (email);
-----------------------------------------------------------------------------------------------------
-- /USERS -------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------


-----------------------------------------------------------------------------------------------------
-- DEVELOPER ACCESS GRANTS --------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------
CREATE TABLE developer_access_grants (
  id                    serial primary key,
  created               timestamp                                NOT NULL DEFAULT CURRENT_TIMESTAMP,
  name                  varchar(255)                             NOT NULL,
  key                   varchar(255)                             NOT NULL UNIQUE,
  developer_id          integer REFERENCES developers (id)       ON DELETE CASCADE NOT NULL,
  UNIQUE (name, developer_id)
);
CREATE INDEX developer_access_grants_dev_index ON developer_access_grants USING hash (developer_id);
CREATE INDEX developer_access_grants_key_index ON developer_access_grants USING hash (key);
-----------------------------------------------------------------------------------------------------
-- /DEVELOPER ACCESS GRANTS -------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------


-----------------------------------------------------------------------------------------------------
-- APPLICATION ACCESS GRANTS ------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------
CREATE TABLE application_access_grants (
  id                    serial primary key,
  created               timestamp                                NOT NULL DEFAULT CURRENT_TIMESTAMP,
  name                  varchar(255)                             NOT NULL,
  key                   varchar(255)                             NOT NULL UNIQUE,
  application_id        integer REFERENCES applications (id)     ON DELETE CASCADE NOT NULL,
  UNIQUE (name, application_id)
);
CREATE INDEX app_access_grants_app_index ON application_access_grants USING hash (application_id);
CREATE INDEX app_access_grants_key_index ON application_access_grants USING hash (key);
-----------------------------------------------------------------------------------------------------
-- /APPLICATION ACCESS GRANTS -----------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------


-----------------------------------------------------------------------------------------------------
-- SERVICES -----------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------
CREATE TABLE services (
  id                    serial primary key,
  created               timestamp                                NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated               timestamp                                NOT NULL DEFAULT CURRENT_TIMESTAMP,
  name                  varchar(255)                             NOT NULL UNIQUE,
  testing               boolean                                  NOT NULL DEFAULT TRUE,
  capacity_context      varchar(255)                             NOT NULL,
  description           text                                     NOT NULL
);
-----------------------------------------------------------------------------------------------------
-- /SERVICES ----------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------


-----------------------------------------------------------------------------------------------------
-- TEST ACCESS GRANTS -------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------
CREATE TABLE test_access_grants (
  id                    serial primary key,
  created               timestamp                                NOT NULL DEFAULT CURRENT_TIMESTAMP,
  service_id            integer REFERENCES services (id)         ON DELETE CASCADE NOT NULL,
  key                   varchar(255)                             NOT NULL UNIQUE
);
CREATE INDEX test_access_grants_ser_index ON test_access_grants USING hash (service_id);
CREATE INDEX test_access_grants_key_index ON test_access_grants USING hash (key);
-----------------------------------------------------------------------------------------------------
-- /TEST ACCESS GRANTS ------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------


-----------------------------------------------------------------------------------------------------
-- USAGE CONSTRAINTS --------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------
CREATE TABLE usage_constraints (
  id                    serial primary key,
  created               timestamp                                NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated               timestamp                                NOT NULL DEFAULT CURRENT_TIMESTAMP,
  service_id            integer REFERENCES services (id)         ON DELETE CASCADE NOT NULL,
  tier                  int                                      NOT NULL,
  capacity              bigint                                   NOT NULL,
  cost                  integer                                  NOT NULL DEFAULT 0,
  UNIQUE (service_id, capacity),
  UNIQUE (service_id, tier)
);
CREATE INDEX uc_service_id_index ON usage_constraints USING hash (service_id);
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
  usage                 bigint                                    NOT NULL DEFAULT 0,
  developer_id          integer REFERENCES developers (id)        ON DELETE CASCADE NOT NULL,
  usage_constraint_id   integer REFERENCES usage_constraints (id) ON DELETE CASCADE NOT NULL
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
  actor_email           varchar(255)                             NOT NULL,
  interface             varchar(255)                             NOT NULL,
  method                varchar(255)                             NOT NULL,
  args                  text                                     NOT NULL
);
CREATE INDEX logged_actions_actor ON logged_actions USING hash (actor);
CREATE INDEX logged_actions_interface ON logged_actions USING hash (interface);
CREATE INDEX logged_actions_actor_id ON logged_actions USING hash (actor_id);
CREATE INDEX logged_actions_created ON logged_actions (created);
-----------------------------------------------------------------------------------------------------
-- /LOGGED ACTIONS ----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------


-----------------------------------------------------------------------------------------------------
-- DATA SEED ----------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------
INSERT INTO administrators (email, password) 
  VALUES ('nick@demonner.net', '$2a$12$X2mAKZYOY0902KMK9BScc.TVW7aOduZmvlmeq6aLXC38ZiUB3Gthi');

INSERT INTO services (name, capacity_context, description) VALUES ('base', 'Object storage', 'No description yet.');
INSERT INTO services (name, capacity_context, description) VALUES ('messaging', 'Messages sent', 'No description yet.');
INSERT INTO services (name, capacity_context, description) VALUES ('hooks', 'Hooks called', 'No description yet.');
-----------------------------------------------------------------------------------------------------
-- /DATA SEED ---------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------