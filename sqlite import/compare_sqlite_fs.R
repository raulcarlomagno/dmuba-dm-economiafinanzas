library(DBI)
library(RSQLite)
require(dplyr)
require(data.table)

con <- dbConnect(SQLite(), "D:\\maestriadm\\dm economia finanzas\\bankchurn\\dias\\dataset.sqlite", flags = SQLITE_RWC)

#res = dbGetQuery( con,'select * from periodos where foto_mes IN (201712,201801,201802)')

datasets_location <- 'D:\\maestriadm\\dm economia finanzas\\bankchurn\\dias\\'

#periodos <- c(seq(201601, 201612),seq(201701, 201712),seq(201801, 201806))
periodos <- c(201805, 201806)

for(periodo in periodos){
	df_sqlite = dbGetQuery(con, paste0('select * from periodos where foto_mes = ', periodo))
	df_txt <- fread(paste0(datasets_location, periodo,'_dias.txt'), header = TRUE, sep = "\t", data.table = FALSE)
	diff <- anti_join(df_sqlite, df_txt)
	print(paste0(periodo, " - ", nrow(diff)))	
	gc()
}


df_sqlite = dbGetQuery(con, paste0('select * from periodos where foto_mes = ', 201805))
df_txt <- fread(paste0(datasets_location, 201805,'_dias.txt'), header = TRUE, sep = "\t", data.table = FALSE)
class(df_sqlite$clase_ternaria)
unique(df_sqlite$clase_ternaria)
class(df_txt$clase_ternaria)
unique(df_txt$clase_ternaria)
df_txt[df_txt$clase_ternaria == '', "clase_ternaria"] <- NA
diff <- anti_join(df_sqlite, df_txt)


df_sqlite = dbGetQuery(con, paste0('select * from periodos where foto_mes = ', 201806))
df_txt <- fread(paste0(datasets_location, 201806,'_dias.txt'), header = TRUE, sep = "\t", data.table = FALSE)
class(df_sqlite$clase_ternaria)
unique(df_sqlite$clase_ternaria)
class(df_txt$clase_ternaria)
unique(df_txt$clase_ternaria)
df_txt$clase_ternaria <- as.character(NA)
diff <- anti_join(df_sqlite, df_txt)


dbDisconnect(con)

 

#hola <- setdiff(data_train,res)



#while(!dbHasCompleted(res)){
#	chunk <- dbFetch(res, n = 5)
#	print(nrow(chunk))
#}

#dbClearResult(res)



#periodos <- c(seq(201601, 201612),seq(201701, 201712),seq(201801, 201806))
#cat(paste0(".import ", periodos, "_dias.txt periodos"), sep = "\n")
#F <- data.frame(campo = paste0("`", data_structure$field, "` ", ifelse(data_structure$class == 'integer', 'INTEGER', 'REAL'), ','))
#update <- data.frame(com=paste0("UPDATE periodos SET ", data_structure$field, " = NULL WHERE ", data_structure$field, " = '';"))
