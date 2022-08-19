-- Your database schema. Use the Schema Designer at http://localhost:8001/ to add some tables.
CREATE TABLE registrations (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    player_name TEXT NOT NULL,
    date TIMESTAMP WITH TIME ZONE NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    has_key BOOLEAN DEFAULT false NOT NULL,
    player_user UUID DEFAULT NULL
);
CREATE INDEX registrations_created_at_index ON registrations (created_at);
CREATE TABLE logs (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    text TEXT NOT NULL,
    user_id UUID DEFAULT NULL
);
CREATE INDEX logs_created_at_index ON logs (created_at);
CREATE TABLE keyholders (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    key_number INT NOT NULL UNIQUE,
    holder TEXT NOT NULL
);
CREATE TABLE users (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    buhl_id TEXT NOT NULL UNIQUE,
    fullname TEXT NOT NULL,
    nickname TEXT DEFAULT NULL,
    last_login TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
CREATE INDEX logs_user_id_index ON logs (user_id);
ALTER TABLE logs ADD CONSTRAINT logs_ref_user_id FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE NO ACTION;
ALTER TABLE registrations ADD CONSTRAINT registrations_ref_player_user FOREIGN KEY (player_user) REFERENCES users (id) ON DELETE NO ACTION;
