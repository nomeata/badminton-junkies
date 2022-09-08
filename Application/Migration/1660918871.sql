ALTER TABLE logs ADD COLUMN user_id UUID DEFAULT null;
CREATE INDEX logs_user_id_index ON logs (user_id);
ALTER TABLE logs ADD CONSTRAINT logs_ref_user_id FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE NO ACTION;
