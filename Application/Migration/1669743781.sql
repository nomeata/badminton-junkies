ALTER TABLE keyholders ADD COLUMN user_id UUID DEFAULT null;
CREATE INDEX keyholders_user_id_index ON keyholders (user_id);
ALTER TABLE keyholders ADD CONSTRAINT keyholders_ref_user_id FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE NO ACTION;
