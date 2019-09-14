DROP INDEX IF EXISTS unique_name;
CREATE UNIQUE INDEX IF NOT EXISTS unique_name_lower ON "user" (LOWER(name));
