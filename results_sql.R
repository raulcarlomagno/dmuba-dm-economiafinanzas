library(DBI)
library(RSQLite)


get_next_plan <- function(){
	results_connection <- dbConnect(SQLite(), CONFIG$RESULTS_FILE, flags = SQLITE_RWC)
	
	query <- "SELECT * FROM plans WHERE finished IS NULL ORDER BY id LIMIT 1"

	df_query <- dbGetQuery(results_connection, query)
	
	dbDisconnect(results_connection)

	return(df_query)		
}


get_mbo_evolution <- function(plan_id) {
	results_connection <- dbConnect(SQLite(), CONFIG$RESULTS_FILE, flags = SQLITE_RWC)

	query <- paste0("SELECT minutes_taken, profit FROM experiments WHERE plan_id = ", plan_id, " ORDER BY id ASC")

	df_query <- dbGetQuery(results_connection, query)

	dbDisconnect(results_connection)

	return(df_query)
}


insert_experiment_result <- function(experiment_code, minutes_taken, profit, profit_0025, auc_testing, logloss_testing, auc_training, logloss_training, perfect_profit_ratio, perfect_profit_ratio_0025, cutoff, algorithm, estimation, train_periods, test_periods, observation, plan_id, hyperparams) {
	results_connection <- dbConnect(SQLite(), CONFIG$RESULTS_FILE)

	query <- "INSERT INTO experiments (experiment_code, minutes_taken, profit, profit_0025, auc_testing, logloss_testing, auc_training, logloss_training, perfect_profit_ratio, perfect_profit_ratio_0025, cutoff, algorithm, estimation, train_periods, test_periods, observation, finished, plan_id) 
	VALUES ('#experiment_code', '#minutes_taken', '#profit#', '#profit_0025#', '#auc_testing', '#logloss_testing', '#auc_training', '#logloss_training', '#perfect_profit_ratio#', '#perfect_profit_ratio_0025#', '#cutoff', '#algorithm', '#estimation', '#train_periods', '#test_periods', '#observation', datetime('now'), '#plan_id')"

	query <- gsub("#experiment_code", experiment_code, query)
	query <- gsub("#minutes_taken", minutes_taken, query)
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

	df_last_id <- dbGetQuery(results_connection, "SELECT last_insert_rowid()")
	experiment_id <- df_last_id[1, 1]

	hyperparams$train_dir <- NULL
	hyperparams$logging_level <- NULL
	hyperparams$task_type <- NULL
	hyperparams$loss_function <- NULL
		
	query <- "INSERT INTO hyperparams (experiment_id, name, value) VALUES "
	hparamssql <- paste0("(", experiment_id, ", '", names(hyperparams), "', '", hyperparams, "')", collapse = ", ")

	query <- paste0(query, hparamssql)
	dbSendQuery(results_connection, query)

	dbDisconnect(results_connection)

	if (CONFIG$IS_LINUX) {
		#copy db to bucket
		file.copy(CONFIG$RESULTS_FILE, CONFIG$WORK_PATH, overwrite = TRUE)
	}

}

finish_plan_period <- function(plan_id) {
	results_connection <- dbConnect(SQLite(), CONFIG$RESULTS_FILE)
	query <- paste0("UPDATE plans SET finished = datetime('now') WHERE id = ", plan_id)
	dbSendQuery(results_connection, query)
	dbDisconnect(results_connection)
}

save_mbo_evo_plot <- function(plan_id) {
	df_evo <- get_mbo_evolution(plan_id)

	if (nrow(df_evo) > 0) {

		file_name <- paste0(CONFIG$TRAIN_DIR, "mbo_evo_", PLAN_ID, ".jpg")

		tiempoacum <- cumsum(df_evo$minutes_taken / 60)
		metricamax <- cummax(df_evo$profit)
		jpeg(filename = file_name, width = 6, height = 4, units = 'in', res = 300)

		plot(tiempoacum,
		  metricamax,
		  type = "n",
		  main = paste0("MBO evo (iteration: ", nrow(df_evo), ", max profit: ", format_money(max(df_evo$profit)), ")"),
		  xlab = "Hours spent",
		  ylab = "Profit",
		  pch = 19)

		mtext(paste0("plot generated at ", Sys.time()), side = 3, cex = 0.5)
		
		lines(tiempoacum, metricamax, type = "l", col = "red")
		lines(tiempoacum, df_evo$profit, type = "l", col = "green3")

		dev.off()

		if (CONFIG$IS_LINUX) { #copy file to bucket
			file.copy(file_name, CONFIG$WORK_PATH, overwrite = TRUE)
		}
	
	} else {
		print(paste0("no data to plot mbo evo for plan_id ", plan_id))
	}
}