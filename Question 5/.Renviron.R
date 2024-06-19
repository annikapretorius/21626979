library(dbbasic)
conn <- db_connect(db = "psql_datascience")
DBI::dbDisconnect(conn)
