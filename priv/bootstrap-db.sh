psql -d postgres -c "DROP DATABASE IF EXISTS bedrock;"
psql -d postgres -c "CREATE DATABASE bedrock;"

psql -d bedrock  -f "priv/schema.sql" -q