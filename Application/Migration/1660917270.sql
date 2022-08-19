ALTER TABLE registrations ADD COLUMN player_user UUID DEFAULT null;
ALTER TABLE registrations ADD CONSTRAINT registrations_ref_player_user FOREIGN KEY (player_user) REFERENCES users (id) ON DELETE NO ACTION;
