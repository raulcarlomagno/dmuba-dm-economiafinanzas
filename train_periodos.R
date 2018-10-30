rm(list = ls())
invisible(gc())

library(catboost)
library(Metrics)

setwd('D:\\maestriadm\\dm economia finanzas\\bankchurn')
#setwd('/home/rcarlomagno/catboost')

source('config.R')
source('dataset_sql.R')
source('results_sql_wo_mbo.R')
source('functions.R')


plan <- get_next_plan()
while (nrow(plan) > 0) {
	print(paste0("executing plan ", plan$id, " - test: ", plan$test, " - train: ", plan$train))

	train_periods <- strsplit(plan$train, split = ',')[[1]]
	data_train <- get_period(train_periods)

	test_periods <- strsplit(plan$test, split = ',')[[1]]
	data_test <- get_period(test_periods)

	useless_columns <- c('numero_de_cliente', 'foto_mes')

	data_train[, useless_columns] <- NULL
	data_train$target <- ifelse(data_train$clase_ternaria == 'CONTINUA', 0, 1)
	data_train$clase_ternaria <- NULL

	data_test[, useless_columns] <- NULL
	data_test$target <- ifelse(data_test$clase_ternaria == 'BAJA+2', 1, 0)
	data_test$clase_ternaria <- NULL

	target_index <- c(which(names(data_train) == "target"))

	#data_train <- as.data.frame(data_train)
	train_pool <- catboost.load_pool(data = data_train[, - target_index], label = data_train[, target_index])

	#data_test <- as.data.frame(data_test)
	test_pool <- catboost.load_pool(data = data_test[, - target_index], label = data_test[, target_index])

	catboost_train <- function() {

		loss_function <- 'Logloss'
		#if(x$cutoff_in_logloss == 1)
		#loss_function <- paste0(loss_function, ':border=', x$cutoff)

		#https://effectiveml.com/using-grid-search-to-optimise-catboost-parameters.html
		fit_params <- list(
			#iterations = 10,
	#thread_count = 10,
			loss_function = loss_function,
	#loss_function = 'Logloss',
	#custom_loss = c('Logloss', 'AUC'),
			task_type = 'GPU',
	#ignored_features = c(4,9),
			#border_count = x$border_count,
			#depth = x$depth,
			#learning_rate = x$learning_rate,
			#l2_leaf_reg = x$l2_leaf_reg,
	#rsm = x$rsm,
			train_dir = CONFIG$TRAIN_DIR,
			logging_level = 'Verbose'
	#logging_level = 'Silent',
			#random_strength = x$random_strength
	#bagging_temperature = x$bagging_temperature
	)


		train_start_time <- Sys.time()
		model <- catboost.train(train_pool, NULL, fit_params)
		train_end_time <- Sys.time()

		predictions_prob_training <- catboost.predict(model, train_pool, prediction_type = 'Probability')
		predictions_prob_testing <- catboost.predict(model, test_pool, prediction_type = 'Probability')

		invisible(gc())

		cat('Training:', plan$train, '\n')
		cat('Testing:', plan$test, '\n')
		
		cutoffs <- seq(0, 0.1, by = 0.001)
		profits <- sapply(cutoffs, calculate_profit, predictions_prob_testing, data_test$target)
		profit_data <- data.frame(cutoff = cutoffs, profit = profits)
		max_profit_cutoff <- max(profit_data[profit_data$profit == max(profit_data$profit),]$cutoff)
		profit_default_cutoff <- (profit_data[profit_data$cutoff == CONFIG$DEFAULT_CUTOFF,]$profit)

		#profit <- calculate_profit(x$cutoff, predictions_prob_testing, data_test$target)
		profit <- max(profit_data$profit)
		perfect_profit <- sum(data_test$target) * 11700

		#print(paste0("profit: $", format_money(profit)))

		profit_ratio_cutoff <- round(profit / perfect_profit, 5)

		cat('Perfect profit:', format_money(perfect_profit), '\n')
		cat('Max profit cutoff:', max_profit_cutoff, '\n')
		cat('Max profit:', format_money(profit), '\n')
		cat('Profit 0.025:', format_money(profit_default_cutoff), '\n')

		#print(paste('Perfect profit ratio cutoff', profit_ratio_cutoff))

		#cat('Perfect profit ratio cutoff', x$cutoff, ':', profit_ratio_cutoff, '\n')
		cat('Perfect profit ratio cutoff 0.025:', round(profit_default_cutoff / perfect_profit, 5), '\n')
		cat('Perfect profit ratio cutoff', max_profit_cutoff, ':', round(max(profit_data$profit) / perfect_profit, 5), '\n')


		auc_training <- round(auc(data_train$target, predictions_prob_training), 5)
		auc_testing <- round(auc(data_test$target, predictions_prob_testing), 5)
		logloss_training <- round(logLoss(data_train$target, predictions_prob_training), 5)
		logloss_testing <- round(logLoss(data_test$target, predictions_prob_testing), 5)

		cat('AUC training:', auc_training, '\n')
		cat('AUC testing:', auc_testing, '\n')
		cat('LogLoss training:', logloss_training, '\n')
		cat('LogLoss testing:', logloss_testing, '\n')
		

		insert_experiment_result(
			'plans',
			round(difftime(train_end_time, train_start_time, units = "secs")),
			profit,
			profit_default_cutoff,
			auc_testing,
			logloss_testing,
			auc_training,
			logloss_training,
			profit_ratio_cutoff,
			round(profit_default_cutoff / perfect_profit, 5),
			max_profit_cutoff,
			'catboost',
			'no cv',
			plan$train,
			plan$test,
			'',
			plan$id
		)
		
	}

	catboost_train()


	finish_plan_period(plan$id)
	plan <- get_next_plan()
}

print("no more plan to execute")
#quit(save = "no")