SET LOCAL client_min_messages = warning;
CREATE UNIQUE INDEX IF NOT EXISTS unique_name_lower ON "user" (LOWER(name));
