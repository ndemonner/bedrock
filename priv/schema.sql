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
  developer_id          int     REFERENCES developers (id)       ON DELETE CASCADE NOT NULL,
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
  application_id        int     REFERENCES applications (id)     ON DELETE CASCADE NOT NULL,
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
  developer_id          int     REFERENCES developers (id)       ON DELETE CASCADE NOT NULL,
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
  application_id        int     REFERENCES applications (id)     ON DELETE CASCADE NOT NULL,
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
  service_id            int     REFERENCES services (id)         ON DELETE CASCADE NOT NULL,
  key                   varchar(255)                             NOT NULL UNIQUE
);
CREATE INDEX test_access_grants_ser_index ON test_access_grants USING hash (service_id);
CREATE INDEX test_access_grants_key_index ON test_access_grants USING hash (key);
-----------------------------------------------------------------------------------------------------
-- /TEST ACCESS GRANTS ------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------


-----------------------------------------------------------------------------------------------------
-- CONSTRAINTS --------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------
CREATE TABLE constraints (
  id                    serial primary key,
  created               timestamp                                NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated               timestamp                                NOT NULL DEFAULT CURRENT_TIMESTAMP,
  service_id            int     REFERENCES services (id)         ON DELETE CASCADE NOT NULL,
  tier                  int                                      NOT NULL,
  capacity              bigint                                   NOT NULL,
  cost                  int                                      NOT NULL DEFAULT 0,
  UNIQUE (service_id, capacity),
  UNIQUE (service_id, tier)
);
CREATE INDEX constraint_service_id_index ON constraints USING hash (service_id);
-----------------------------------------------------------------------------------------------------
-- /CONSTRAINTS -------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------


-----------------------------------------------------------------------------------------------------
-- SUBSCRIPTIONS ------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------
CREATE TABLE subscriptions (
  id                    serial primary key,
  created               timestamp                                 NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated               timestamp                                 NOT NULL DEFAULT CURRENT_TIMESTAMP,
  developer_id          int     REFERENCES developers (id)        ON DELETE CASCADE NOT NULL,
  constraint_id         int     REFERENCES constraints (id)       ON DELETE CASCADE NOT NULL,
  service_id            int     REFERENCES services (id)          NOT NULL
);
CREATE INDEX sub_developer_id ON subscriptions USING hash (developer_id);
CREATE INDEX sub_constraint_id ON subscriptions USING hash (constraint_id);
CREATE INDEX sub_service_id ON subscriptions USING hash (service_id);
-----------------------------------------------------------------------------------------------------
-- /SUBSCRIPTIONS -----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------


-----------------------------------------------------------------------------------------------------
-- DATA SEED ----------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------
INSERT INTO administrators (email, password) 
  VALUES ('nick@demonner.net', '$2a$12$X2mAKZYOY0902KMK9BScc.TVW7aOduZmvlmeq6aLXC38ZiUB3Gthi');

INSERT INTO services (name, capacity_context, description) VALUES ('base', 'total storage in bytes', 'No description yet.');
INSERT INTO services (name, capacity_context, description) VALUES ('messaging', 'number of messages sent per month', 'No description yet.');
INSERT INTO services (name, capacity_context, description) VALUES ('hooks', 'number of hooks called per month', 'No description yet.');
-----------------------------------------------------------------------------------------------------
-- /DATA SEED ---------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------