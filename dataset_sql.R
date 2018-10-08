library(DBI)
library(RSQLite)

get_period <- function(periods){
	dataset_connection <- dbConnect(SQLite(), CONFIG$DATASET_FILE, flags = SQLITE_RWC)
	df_query <- dbGetQuery(dataset_connection, paste0('SELECT * FROM periodos WHERE foto_mes IN (', paste(periods, collapse = ', '), ')'))
	dbDisconnect(dataset_connection)
	return(df_query)		
}



