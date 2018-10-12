library(DBI)
library(RSQLite)


get_relative_date_sentence <- function(months) {
	return(paste0("STRFTIME('%Y%m', DATE(SUBSTR(p.foto_mes, 1, 4) || '-' || SUBSTR(p.foto_mes, 5, 2) || '-01', '", months, " month', 'start of month'))"))
}

get_period <- function(periods, history_fields = FALSE){
	dataset_connection <- dbConnect(SQLite(), CONFIG$DATASET_FILE, flags = SQLITE_RWC)
	
	query <- "SELECT 1"

	if (history_fields) {
		calculated_fields <- c(query)

		calculated_fields <- c(calculated_fields, paste0("(SELECT AVG(coalesce(p1.mplan_sueldo, 0)) FROM periodos p1 WHERE p1.numero_de_cliente = p.numero_de_cliente AND p1.foto_mes < p.foto_mes AND p1.foto_mes > 201709) as _mplan_sueldo_avg6"))
		#calculated_fields <- c(calculated_fields, paste0("(SELECT AVG(coalesce(p1.mplan_sueldo, 0)) FROM periodos p1 WHERE p1.numero_de_cliente = p.numero_de_cliente AND p1.foto_mes < p.foto_mes AND p1.foto_mes > ", get_relative_date_sentence(-3), ") as _mplan_sueldo_avg3"))

		query <- paste0(calculated_fields, collapse = ", ")
	}

	query <- paste(query, "FROM periodos p")
	query <- paste(query, "WHERE p.foto_mes IN (", paste(periods, collapse = ', '), ")")

	#print(query)
	df_query <- dbGetQuery(dataset_connection, query)
	
	dbDisconnect(dataset_connection)

	return(df_query)		
}



