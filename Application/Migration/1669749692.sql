ALTER TABLE keyholders DROP COLUMN holder;
ALTER TABLE keyholders ALTER COLUMN user_id SET NOT NULL;
