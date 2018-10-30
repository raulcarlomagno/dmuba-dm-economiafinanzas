library(DBI)
library(RSQLite)


get_next_plan <- function(){
	results_connection <- dbConnect(SQLite(), CONFIG$RESULTS_FILE_WO_MBO, flags = SQLITE_RWC)
	
	query <- "SELECT * FROM plans WHERE finished IS NULL ORDER BY id LIMIT 1"

	df_query <- dbGetQuery(results_connection, query)
	
	dbDisconnect(results_connection)

	return(df_query)		
}

insert_experiment_result <- function(experiment_code, seconds_taken, profit, profit_0025, auc_testing, logloss_testing, auc_training, logloss_training, perfect_profit_ratio, perfect_profit_ratio_0025, cutoff, algorithm, estimation, train_periods, test_periods, observation, plan_id) {
	results_connection <- dbConnect(SQLite(), CONFIG$RESULTS_FILE_WO_MBO)

	query <- "INSERT INTO experiments (experiment_code, seconds_taken, profit, profit_0025, auc_testing, logloss_testing, auc_training, logloss_training, perfect_profit_ratio, perfect_profit_ratio_0025, cutoff, algorithm, estimation, train_periods, test_periods, observation, finished, plan_id) 
	VALUES ('#experiment_code', '#seconds_taken', '#profit#', '#profit_0025#', '#auc_testing', '#logloss_testing', '#auc_training', '#logloss_training', '#perfect_profit_ratio#', '#perfect_profit_ratio_0025#', '#cutoff', '#algorithm', '#estimation', '#train_periods', '#test_periods', '#observation', datetime('now'), '#plan_id')"

	query <- gsub("#experiment_code", experiment_code, query)
	query <- gsub("#seconds_taken", seconds_taken, query)
	query <- gsub("#profit#", profit, query)
	query <- gsub("#profit_0025#", profit_0025, query)
	query <- gsub("#auc_testing", auc_testing, query)
	query <- gsub("#logloss_testing", logloss_testing, query)
	query <- gsub("#auc_training", auc_training, query)
	query <- gsub("#logloss_training", logloss_training, query)
	query <- gsub("#perfect_profit_ratio#", perfect_profit_ratio, query)
	query <- gsub("#perfect_profit_ratio_0025#", perfect_profit_ratio_0025, query)
	query <- gsub("#cutoff", cutoff, query)
	query <- gsub("#algorithm", algorithm, query)
	query <- gsub("#estimation", estimation, query)
	query <- gsub("#train_periods", train_periods, query)
	query <- gsub("#test_periods", test_periods, query)
	query <- gsub("#observation", observation, query)
	query <- gsub("#plan_id", plan_id, query)	

	dbSendQuery(results_connection, query)

	dbDisconnect(results_connection)
}

finish_plan_period <- function(plan_id) {
	results_connection <- dbConnect(SQLite(), CONFIG$RESULTS_FILE_WO_MBO)
	query <- paste0("UPDATE plans SET finished = datetime('now') WHERE id = ", plan_id)
	dbSendQuery(results_connection, query)
	dbDisconnect(results_connection)
}
