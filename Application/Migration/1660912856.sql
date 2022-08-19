CREATE TABLE users (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    "buhl-id" TEXT NOT NULL,
    fullname TEXT NOT NULL,
    nickname TEXT DEFAULT null
);
ALTER TABLE users ADD CONSTRAINT "users_buhl-id_key" UNIQUE(buhl-id);
