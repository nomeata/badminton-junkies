CREATE TABLE keyholders (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    key_number INT NOT NULL,
    holder TEXT NOT NULL
);
ALTER TABLE keyholders ADD CONSTRAINT keyholders_key_number_key UNIQUE(key_number);
