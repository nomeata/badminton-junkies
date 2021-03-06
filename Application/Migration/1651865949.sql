CREATE TABLE registrations (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    player_name TEXT NOT NULL,
    date TIMESTAMP WITH TIME ZONE NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL
);
CREATE INDEX registrations_created_at_index ON registrations (created_at);
CREATE TABLE logs (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT now() NOT NULL,
    text TEXT NOT NULL
);
CREATE INDEX logs_created_at_index ON logs (created_at);
