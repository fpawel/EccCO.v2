module initdb

let sql = """
PRAGMA foreign_keys = ON;
PRAGMA encoding = 'UTF-8';
--E:\Program Data\Аналитприбор\elco\elco.sqlite
--C:\Users\fpawel\AppData\Roaming\Аналитприбор\elco\elco.sqlite

CREATE TABLE IF NOT EXISTS gas
(
  gas_name TEXT PRIMARY KEY NOT NULL,
  code     INTEGER UNIQUE   NOT NULL
);

CREATE TABLE IF NOT EXISTS units
(
  units_name TEXT PRIMARY KEY,
  code       INTEGER UNIQUE NOT NULL
);

CREATE TABLE IF NOT EXISTS product_type
(
  product_type_name   TEXT PRIMARY KEY NOT NULL,
  display_name        TEXT,
  gas_name            TEXT             NOT NULL,
  units_name          TEXT             NOT NULL,
  scale               REAL             NOT NULL,
  noble_metal_content REAL             NOT NULL,
  lifetime_months     INTEGER          NOT NULL CHECK (lifetime_months > 0),
  lc64                BOOLEAN          NOT NULL
    CONSTRAINT boolean_lc64
      CHECK (lc64 IN (0, 1)),
  points_method       INTEGER          NOT NULL
    CONSTRAINT points_method_2_or_3
      CHECK (points_method IN (2, 3)),
  max_fon             REAL,
  max_d_fon           REAL,
  min_k_sens20        REAL,
  max_k_sens20        REAL,
  min_d_temp          REAL,
  max_d_temp          REAL,
  min_k_sens50        REAL,
  max_k_sens50        REAL,
  max_d_not_measured  REAL,
  FOREIGN KEY (gas_name) REFERENCES gas (gas_name),
  FOREIGN KEY (units_name) REFERENCES units (units_name)
);

CREATE TABLE IF NOT EXISTS party
(
  party_id          INTEGER PRIMARY KEY NOT NULL,
  old_party_id      TEXT,
  created_at        TIMESTAMP           NOT NULL DEFAULT (datetime('now')),
  updated_at        TIMESTAMP           NOT NULL DEFAULT (datetime('now')),
  product_type_name TEXT                NOT NULL DEFAULT '035',
  concentration1    REAL                NOT NULL DEFAULT 0 CHECK (concentration1 >= 0),
  concentration2    REAL                NOT NULL DEFAULT 50 CHECK (concentration2 >= 0),
  concentration3    REAL                NOT NULL DEFAULT 100 CHECK (concentration3 >= 0),
  note              TEXT,
  FOREIGN KEY (product_type_name) REFERENCES product_type (product_type_name)
);

CREATE TABLE IF NOT EXISTS product
(
  product_id        INTEGER PRIMARY KEY NOT NULL,
  party_id          INTEGER             NOT NULL,
  serial            INTEGER,
  place             INTEGER             NOT NULL CHECK (place >= 0),
  product_type_name TEXT,
  note              TEXT,

  i_f_minus20       REAL,
  i_f_plus20        REAL,
  i_f_plus50        REAL,

  i_s_minus20       REAL,
  i_s_plus20        REAL,
  i_s_plus50        REAL,

  i13               REAL,
  i24               REAL,
  i35               REAL,
  i26               REAL,
  i17               REAL,
  not_measured      REAL,
  flash             BLOB,
  production        BOOLEAN             NOT NULL CHECK (production IN (0, 1)) DEFAULT 0,

  old_product_id    TEXT,
  old_serial        INTEGER,

  CONSTRAINT unique_party_place UNIQUE (party_id, place),
  CONSTRAINT unique_party_serial UNIQUE (party_id, serial),

  FOREIGN KEY (product_type_name) REFERENCES product_type (product_type_name),
  FOREIGN KEY (party_id) REFERENCES party (party_id)
    ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS product_temperature_current_k_sens
(
  product_id  INTEGER NOT NULL,
  temperature REAL    NOT NULL,
  current     REAL,
  k_sens      REAL,
  CONSTRAINT product_temperature_current_k_sens_primary_key UNIQUE (product_id, temperature),
  FOREIGN KEY (product_id) REFERENCES product (product_id)
    ON DELETE CASCADE
);

CREATE TRIGGER IF NOT EXISTS trigger_product_party_updated_at
  AFTER INSERT
  ON product
  BEGIN
    UPDATE party
    SET updated_at = datetime('now')
    WHERE party.party_id = new.party_id;
  END;

CREATE TRIGGER IF NOT EXISTS trigger_party_updated_at
  BEFORE UPDATE
  ON party
  BEGIN
    UPDATE party SET updated_at = (datetime('now')) WHERE party_id = old.party_id;
  END;


CREATE VIEW IF NOT EXISTS party_info AS
SELECT *,
       cast(strftime('%Y', created_at) AS INTEGER) AS year,
       cast(strftime('%m', created_at) AS INTEGER) AS month,
       cast(strftime('%d', created_at) AS INTEGER) AS day
FROM party;

CREATE VIEW IF NOT EXISTS last_party AS
SELECT *
FROM party
ORDER BY created_at DESC
LIMIT 1;


CREATE VIEW IF NOT EXISTS product_info AS
  WITH q1 AS (SELECT product_id,
                     i_f_plus20,
                     (CASE (product.product_type_name ISNULL)
                        WHEN 1 THEN party.product_type_name
                        WHEN 0 THEN product.product_type_name END)                           AS product_type_name,
                     round(100 * (i_s_plus50 - i_f_plus50) / (i_s_plus20 - i_f_plus20), 3)   AS k_sens50,
                     round((i_s_plus20 - i_f_plus20) / (concentration3 - concentration1), 3) AS k_sens20,
                     round(i13 - i_f_plus20, 3)                                              AS d_fon20,
                     round(i_f_plus50 - i_f_plus20, 3)                                       AS d_fon50,
                     round(not_measured - i_f_plus20, 3)                                     AS d_not_measured
              FROM product
                     INNER JOIN party ON party.party_id = product.party_id

    ), q2 AS (
    SELECT q1.product_id,
           abs(d_not_measured) < max_d_not_measured            AS ok_d_not_measured,
           abs(d_fon50) < max_d_temp                           AS ok_d_fon50,
           abs(d_fon20) < max_d_fon                            AS ok_d_fon20,
           k_sens20 < max_k_sens20 AND k_sens20 > min_k_sens20 AS ok_k_sens20,
           k_sens50 < max_k_sens50 AND k_sens50 > min_k_sens50 AS ok_k_sens50,
           i_f_plus20 < max_fon                                AS ok_fon20
    FROM q1
           INNER JOIN product_type ON product_type.product_type_name = q1.product_type_name
    ), q3 AS (
    SELECT product_id,
           NOT (ok_d_not_measured AND ok_d_fon50 AND ok_d_fon20 AND ok_k_sens20 AND ok_k_sens50 AND
                ok_fon20) AS not_ok
    FROM q2
    )
    SELECT q1.product_id,
           product.party_id,
           place,
           serial,
           product.old_product_id,
           q1.product_type_name,
           product.product_type_name      AS self_product_type_name,
           product.note,
           gas_name,
           units_name,
           scale,
           noble_metal_content,
           lifetime_months,
           lc64,
           points_method,

           round(i_f_minus20, 3)          AS i_f_minus20,
           round(q1.i_f_plus20, 3)        AS i_f_plus20,
           round(i_f_plus50, 3)           AS i_f_plus50,
           round(i_s_minus20, 3)          AS i_s_minus20,
           round(i_s_plus20, 3)           AS i_s_plus20,
           round(i_s_plus50, 3)           AS i_s_plus50,

           party.concentration1,
           party.concentration3,
           round(product.not_measured, 3) AS not_measured,


           round(product.i13, 3)          AS i13,

           round(i24, 3)                  AS i24,
           round(i35, 3)                  AS i35,
           round(i26, 3)                  AS i26,
           round(i17, 3)                  AS i17,
           flash NOT NULL                 AS firmware,

           k_sens20,
           k_sens50,
           d_fon50,
           d_fon20,
           d_not_measured,

           ok_fon20,
           ok_d_fon20,
           ok_k_sens20,
           ok_d_fon50,
           ok_k_sens50,
           ok_d_not_measured,

           production,
           not_ok

    FROM q1
           INNER JOIN product_type ON product_type.product_type_name = q1.product_type_name
           INNER JOIN q2 ON q2.product_id = q1.product_id
           INNER JOIN q3 ON q3.product_id = q1.product_id
           INNER JOIN product ON product.product_id = q1.product_id
           INNER JOIN party ON party.party_id = product.party_id;


INSERT
  OR
REPLACE INTO units (units_name, code)
VALUES ('мг/м3', 2),
       ('ppm', 3),
       ('об. дол. %', 7),
       ('млн-1', 5);

INSERT
  OR
REPLACE INTO gas (gas_name, code)
VALUES ('CO', 0x11),
       ('H₂S', 0x22),
       ('NH₃', 0x33),
       ('Cl₂', 0x44),
       ('SO₂', 0x55),
       ('NO₂', 0x66),
       ('O₂', 0x88),
       ('NO', 0x99),
       ('HCl', 0xAA);

INSERT
  OR
REPLACE INTO product_type (product_type_name,
                           display_name,
                           gas_name,
                           units_name,
                           scale,
                           noble_metal_content,
                           lifetime_months,
                           lc64,
                           points_method,
                           max_fon,
                           max_d_fon,
                           min_k_sens20,
                           max_k_sens20,
                           min_d_temp,
                           max_d_temp,
                           min_k_sens50,
                           max_k_sens50,
                           max_d_not_measured)
VALUES ('035', '035', 'CO', 'мг/м3', 200, 0.1626, 18, 0, 3, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL),
       ('035(2)', '035', 'CO', 'мг/м3', 200, 0.1456, 12, 0, 3, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL),
       ('035-59',
        NULL,
        'CO',
        'об. дол. %',
        0.5,
        0.1891,
        12,
        0,
        3,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL),
       ('035-60', NULL, 'CO', 'мг/м3', 200, 0.1891, 12, 0, 3, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL),
       ('035-61', NULL, 'CO', 'ppm', 2000, 0.1891, 12, 0, 3, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL),
       ('035-80', NULL, 'CO', 'мг/м3', 200, 0.1456, 12, 0, 3, 1.01, 3, 0.08, 0.175, 0, 2, 100, 135, 5),
       ('035-81', NULL, 'CO', 'мг/м3', 1500, 0.1456, 12, 0, 3, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL),
       ('035-92',
        NULL,
        'CO',
        'об. дол. %',
        0.5,
        0.1891,
        12,
        0,
        3,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL),
       ('035-93', NULL, 'CO', 'млн-1', 200, 0.1891, 12, 0, 3, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL),
       ('035-94', NULL, 'CO', 'млн-1', 2000, 0.1891, 12, 0, 3, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL),
       ('035-105', NULL, 'CO', 'мг/м3', 200, 0.1456, 12, 0, 3, 1.51, 3, 0.08, 0.335, 0, 3, 110, 145.01, NULL),
       ('100', NULL, 'CO', 'мг/м3', 200, 0.0816, 12, 1, 3, 1, 3, 0.08, 0.175, 0, 3, 100, 135, NULL),
       ('100-05', NULL, 'CO', 'мг/м3', 50, 0.0816, 12, 1, 3, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL),
       ('100-10', NULL, 'CO', 'мг/м3', 200, 0.0816, 12, 1, 3, 1, 3, 0.08, 0.175, 0, 3, 100, 135, NULL),
       ('100-15', NULL, 'CO', 'мг/м3', 50, 0.0816, 12, 1, 3, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL),
       ('035-40', NULL, 'CO', 'мг/м3', 200, 0.1456, 12, 0, 2, 1.51, 3, 0.08, 0.335, 0, 3, 110, 145.1, NULL),
       ('035-21', NULL, 'CO', 'мг/м3', 200, 0.1456, 12, 0, 2, 1.51, 3, 0.08, 0.335, 0, 3, 110, 145.01, NULL),
       ('130-01', NULL, 'CO', 'мг/м3', 200, 0.1626, 12, 0, 3, 1.01, 3, 0.08, 0.18, 0, 2, 100, 135, 5),
       ('035-70', NULL, 'CO', 'мг/м3', 200, 0.1626, 12, 0, 2, 1.51, 3, 0.08, 0.33, 0, 3, 110, 146, NULL),
       ('130-08', NULL, 'CO', 'ppm', 100, 0.1162, 12, 0, 3, 1, 3, 0.08, 0.18, 0, 2, 100, 135, 5),
       ('035-117', NULL, 'NO₂', 'мг/м3', 200, 0.1626, 18, 1, 3, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL),
       ('010-18', NULL, 'O₂', 'об. дол. %', 21, 0, 12, 1, 3, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL),
       ('010-18', NULL, 'O₂', 'об. дол. %', 21, 0, 12, 1, 3, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL),
       ('035-111', NULL, 'CO', 'мг/м3', 200, 0.1626, 12, 1, 3, 1, 3, 0.08, 0.175, 0, 3, 100, 135, 5);


DELETE
FROM party
WHERE NOT EXISTS(SELECT product_id FROM product WHERE party.party_id = product.party_id);
"""

