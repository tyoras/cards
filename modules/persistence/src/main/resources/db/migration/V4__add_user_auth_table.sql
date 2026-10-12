CREATE TABLE IF NOT EXISTS user_auth
(
  user_id       UUID PRIMARY KEY REFERENCES users (id) ON DELETE CASCADE,
  created_at    TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL,
  updated_at    TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL,
  password_hash bytea                                  NOT NULL,
  password_salt bytea                                  NOT NULL
);
