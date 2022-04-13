DO
$$
    BEGIN
        IF NOT EXISTS(SELECT 1 FROM pg_type WHERE typname = 'game_type') THEN
            CREATE TYPE game_type AS ENUM
                (
                    'schnapsen',
                    'war'
                    );
        END IF;
    END
$$;


CREATE TABLE IF NOT EXISTS games
(
    id         UUID PRIMARY KEY         DEFAULT uuid_generate_v4(),
    created_at TIMESTAMP WITH TIME ZONE DEFAULT now()       NOT NULL,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT now()       NOT NULL,
    game_type  game_type                                    NOT NULL,
    state      JSONB                    DEFAULT '{}'::JSONB NOT NULL
);

CREATE TABLE IF NOT EXISTS gamesplayers
(
    game_id   UUID NOT NULL,
    player_id UUID NOT NULL,
    FOREIGN KEY (game_id) REFERENCES games (id) ON DELETE CASCADE,
    FOREIGN KEY (player_id) REFERENCES users (id) ON DELETE CASCADE,
    UNIQUE (game_id, player_id)
);