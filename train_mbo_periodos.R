rm(list = ls())
invisible(gc())

library(catboost)
library(mlrMBO)
library(Metrics)

setwd('D:\\maestriadm\\dm economia finanzas\\bankchurn')
#setwd('/home/rcarlomagno/catboost')

source('config.R')
source('dataset_sql.R')
source('results_sql.R')
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

	catboost_train <- function(x = list(
				depth,
				iterations,
				border_count,
				l2_leaf_reg,
				learning_rate,
#rsm,
				cutoff,
#cutoff_in_logloss,
				random_strength
#bagging_temperature
)) {

		loss_function <- 'Logloss'
		#if(x$cutoff_in_logloss == 1)
		#loss_function <- paste0(loss_function, ':border=', x$cutoff)

		#https://effectiveml.com/using-grid-search-to-optimise-catboost-parameters.html
		fit_params <- list(
			iterations = x$iterations,
	#thread_count = 10,
			loss_function = loss_function,
	#loss_function = 'Logloss',
	#custom_loss = c('Logloss', 'AUC'),
			task_type = 'GPU',
	#ignored_features = c(4,9),
			border_count = x$border_count,
			depth = x$depth,
			learning_rate = x$learning_rate,
			l2_leaf_reg = x$l2_leaf_reg,
	#rsm = x$rsm,
			train_dir = CONFIG$TRAIN_DIR,
			logging_level = 'Verbose',
	#logging_level = 'Silent',
			random_strength = x$random_strength
	#bagging_temperature = x$bagging_temperature
	)

		cat("training with these hyperparameters:\n")
		print(data.frame(x), row.names = FALSE)

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
		profit_default_cutoff <- profit_data[profit_data$cutoff == CONFIG$DEFAULT_CUTOFF,]$profit

		profit <- calculate_profit(x$cutoff, predictions_prob_testing, data_test$target)
		perfect_profit <- sum(data_test$target) * 11700

		print(paste0("profit: $", format_money(profit)))

		profit_ratio_cutoff <- round(profit / perfect_profit, 5)
		
		cat('Max profit cutoff:', max_profit_cutoff, '\n')
		cat('Max profit:', format_money(max(profit_data)), '\n')
		cat('Profit 0.025:', format_money(profit_default_cutoff), '\n')

		print(paste('Perfect profit ratio cutoff', profit_ratio_cutoff))

		cat('Perfect profit ratio cutoff', x$cutoff, ':', profit_ratio_cutoff, '\n')
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
			difftime(train_end_time, train_start_time, units = "mins"),
			profit,
			profit_default_cutoff,
			auc_testing,
			logloss_testing,
			auc_training,
			logloss_training,
			profit_ratio_cutoff,
			round(profit_default_cutoff / perfect_profit, 5),
			x$cutoff,
			'catboost',
			'no cv',
			plan$train,
			plan$test,
			'',
			plan$id,
			fit_params
		)
		
		return(profit_ratio_cutoff)
	}


	configureMlr(show.learner.output = FALSE)

	#configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
	#https://github.com/catboost/benchmarks/blob/master/quality_benchmarks/comparison_description.pdf
	objetive_function <- makeSingleObjectiveFunction(
		name = "catboost_optimizer",
		fn = catboost_train,
		par.set = makeParamSet(
				makeIntegerParam("depth", lower = 1L, upper = 10L), #dice q depth lo haga de 6 a 10
makeIntegerParam("iterations", lower = 250L, upper = 1250L),
				#makeIntegerParam("iterations", lower = 1L, upper = 2L),
				makeIntegerParam("border_count", lower = 2L, upper = 254L), #subir fix para avisar que con 255 no va. Error in catboost.train(train_pool, NULL, fit_params) : c:/goagent/pipelines/buildmaster/catboost.git/catboost/cuda/gpu_data/compressed_index_builder.h:110: Error: can't proceed some features ,  Error: border count should be greater than 0. If you have nan-features, border count should be > 1. Got 1"
				#border_count no lo optimiza
				makeIntegerParam("l2_leaf_reg", lower = 1, upper = 10),
				makeNumericParam("learning_rate", lower = 1e-07, upper = 1), #o 1e-06
#makeNumericParam("rsm", lower = 0.01, upper = 1), #rsm on GPU is supported for pairwise modes only
				makeNumericParam("cutoff", lower = 0.01, upper = 0.1),
#makeIntegerParam("cutoff_in_logloss", lower = 0L, upper = 1L), #la regresion Kriging no soporta makeLogicalParam
				makeIntegerParam("random_strength", lower = 1, upper = 20)
#makeIntegerParam("bagging_temperature", lower = 0, upper = 1)
				#bagging_temperature lo optimiza!!
		),
		minimize = FALSE,
		has.simple.signature = FALSE,
		global.opt.value = -1
	)


	mbo_iterations = 200

	mbo_control <- makeMBOControl(propose.points = 1L)
	mbo_control <- setMBOControlTermination(mbo_control, iters = mbo_iterations)
	mbo_control <- setMBOControlInfill(mbo_control, crit = makeMBOInfillCritEI(), opt = "focussearch", opt.focussearch.points = mbo_iterations)

	#matern3_2
	surrogate_learner <- makeLearner("regr.km", predict.type = "se", covtype = "matern5_2", control = list(trace = FALSE))
	mbo_learner <- makeMBOLearner(mbo_control, objetive_function)

	mbo_start_time <- Sys.time()
	mbo_result <- mbo(objetive_function, learner = surrogate_learner, control = mbo_control, design = NULL)
	mbo_end_time <- Sys.time()

	print("time elapsed doing mbo:")
	print(mbo_end_time - mbo_start_time)

	cat("best hyperparameters for current plan:\n")
	print(data.frame(mbo_result$x), row.names = FALSE)
	#print(paste0("profit: $", formatC(mbo_result$y, format="f", digits=0, big.mark=".", decimal.mark = ',')))
	print(paste0("Perfect profit ratio cutoff: ", mbo_result$y))

	png(paste0(CONFIG$TRAIN_DIR, "mbo", plan$id ,".png"), width = 1920, height = 1080)
	plot(mbo_result)
	dev.off()

	finish_plan_period(plan$id)
	plan <- get_next_plan()
}

print("no more plan to execute")
#quit(save = "no")