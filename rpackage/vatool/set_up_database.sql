
CREATE TABLE vatool_users (
  user_id INT,
  username VARCHAR(16),
  password VARCHAR(16),
  first_name VARCHAR(32),
  last_name VARCHAR(32),
  country VARCHAR(16),
  role VARCHAR(16)
);

CREATE TABLE vatool_sessions (
    user_id INT,
    start_time   TIMESTAMP,
    end_time   TIMESTAMP
);

CREATE TABLE vatool_cods (
  user_id INT,
  death_id TEXT,
  cod_code_1 TEXT,
  cod_1 TEXT,
  cod_code_2 TEXT,
  cod_2 TEXT,
  cod_code_3 TEXT,
  cod_3 TEXT,
  time_stamp TIMESTAMP,
  doctor_note TEXT
);