.PHONY: all

DB_NAME = image
PG_USER = image_user
PG_PORT = 5432

all: createschema

createuser:
	createuser $(PG_USER) --createdb --superuser

createdb: createuser
	createdb $(DB_NAME) -U $(PG_USER) -p $(PG_PORT)

createschema: createdb
	psql $(DB_NAME) -f db.sql

clean:
	dropdb $(DB_NAME)
	dropuser $(PG_USER)
