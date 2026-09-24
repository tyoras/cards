CREATE TABLE IF NOT EXISTS games_stats
(
  id         UUID PRIMARY KEY         DEFAULT uuid_generate_v4(),
  created_at TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL,
  updated_at TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL,
  player_id  UUID REFERENCES users (id) ON DELETE CASCADE,
  game_type  game_type                              NOT NULL,
  won        INT                      DEFAULT 0     NOT NULL
    CONSTRAINT won_must_be_positive CHECK (won >= 0),
  draw       INT                      DEFAULT 0     NOT NULL
    CONSTRAINT draw_must_be_positive CHECK (draw >= 0),
  lost       INT                      DEFAULT 0     NOT NULL
    CONSTRAINT lost_must_be_positive CHECK (lost >= 0),
  UNIQUE (player_id, game_type)
);

CREATE INDEX IF NOT EXISTS idx_games_stats_player_id ON games_stats (player_id);
CREATE INDEX IF NOT EXISTS idx_games_stats_game_type ON games_stats (game_type);
