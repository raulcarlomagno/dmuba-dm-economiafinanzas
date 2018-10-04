rm(list = ls())
gc()

library(catboost)
library(data.table)
library(dplyr)
library(MLmetrics)
library(ggplot2)
#library( "DiceKriging" )
library(mlrMBO)



datasets_location <- 'D:\\maestriadm\\dm economia finanzas\\bankchurn\\dias\\'

memory.limit(4096)
train_periods <- c(201802)
#train_periods <- c(201801, 201712, 201711)

data_train <- do.call(rbind, lapply(train_periods, function(period) fread(paste0(datasets_location, period,'_dias.txt'), header = TRUE, sep = "\t")))
data_train <- data_train[sample(1:nrow(data_train)), ] 

glimpse(data_train)

data_test <- fread(paste0(datasets_location, '201804_dias.txt'), header = TRUE, sep = "\t")

data_structure <- data.frame(class = sapply(data_train, class))
data_structure$field <- rownames(data_structure)
rownames(data_structure) <- NULL

data_structure %>% 
	group_by(class) %>% 
	summarise(n = n())

useless_columns <- c('numero_de_cliente',
'foto_mes')
#'tpaquete1',
#'tpaquete2',
#'tpaquete3',
#'tpaquete4',
#'tpaquete5',
#'tpaquete6',
#'tpaquete8',
#'mcuenta_corriente_dolares',
#'cprestamos_hipotecarios',
#'tplazo_fijo',
#'mplazo_fijo_pesos',
#'ttitulos',
#'mbonos_corporativos',
#'mmonedas_extranjeras',
#'minversiones_otras',
#'mplan_sueldo_manual',
#'cplan_sueldo_transaccion',
#'ccuenta_descuentos',
#'mcuenta_descuentos',
#'mtransferencias_recibidas',
#'tautoservicio',
#'cautoservicio_transacciones')

data_train[, useless_columns] <- NULL
data_train$target <- ifelse(data_train$clase_ternaria == 'CONTINUA', 0, 1)
data_train$clase_ternaria <- NULL

data_test[, useless_columns] <- NULL
data_test$target <- ifelse(data_test$clase_ternaria == 'BAJA+2', 1, 0)
data_test$clase_ternaria <- NULL

target_index <- c(which(names(data_train) == "target"))

data_train <- as.data.frame(data_train)
train_pool <- catboost.load_pool(data = data_train[, -target_index], label = data_train[, target_index])

data_test <- as.data.frame(data_test)
test_pool <- catboost.load_pool(data = data_test[, -target_index], label = data_test[, target_index])

#table(is.na(data_test[,80]))
#data_test[is.na(data_test[,80]), 80] <- 0


calculate_profit <- function(cutoff, probabilities, true_classes){
	sum((probabilities > cutoff) * ifelse(true_classes == 1, 11000, -300))
}

catboost_train <- function(x = list(
				depth,
				iterations,
				border_count,
				l2_leaf_reg,
				learning_rate,
				#rsm,
				cutoff,
				cutoff_in_logloss
)){
	
	loss_function <- 'Logloss'
	if(x$cutoff_in_logloss == 1)
		loss_function <- paste0(loss_function, ':border=', x$cutoff)
	
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
			train_dir = 'train_dir',
			logging_level = 'Verbose'
			#logging_level = 'Silent'		
	)
	
	cat("training with these hyperparameters:\n")
	print(data.frame(x), row.names = FALSE )
	
	#model <- catboost.train(train_pool, test_pool, fit_params)
	model <- catboost.train(train_pool, NULL, fit_params)
	
	
	#features_importance <- catboost.get_feature_importance(model, 
	#		pool = train_pool, 
	#		fstr_type = "FeatureImportance",
	#		thread_count = -1)
	#features_importance <- data.frame(feature = attr(features_importance,"names"), importance = features_importance)

	
	#predictions_prob_training <- catboost.predict(model, train_pool, prediction_type = 'Probability')
	predictions_prob_testing <- catboost.predict(model, test_pool, prediction_type = 'Probability') 
	
	invisible(gc())
	
	#cutoffs <- seq(0, 0.1, by = 0.0005)
	##cutoffs <- seq(0, 0.05, by = 0.0005)
	
	#	profits <- sapply(cutoffs, calculate_profit, predictions_prob_testing, data_test$target)
	#	profit_data <- data.frame(cutoff = cutoffs, profit = profits)
	#	
	#	max_profit_cutoff <- profit_data[profit_data$profit == max(profit_data$profit), ]$cutoff
	#	
	#	ggplot(profit_data, aes(x = cutoff, y = profit)) +
	#			geom_line() +
	#			geom_vline(aes(xintercept = max_profit_cutoff), color='red', linetype=2) + 
	#			geom_hline(aes(yintercept = max(profit_data)), color='blue', linetype=2) +
	#			scale_y_continuous("Profit", breaks = sort(c(seq(min(profit_data), max(profit_data), length.out=6)))) +
	#			scale_x_continuous("Cutoff") +
	#			geom_text(aes(max_profit_cutoff, max(profit_data) - (max(profit_data) * 0.2), label = max_profit_cutoff), size = 4) +
	#			geom_text(aes(max_profit_cutoff, max(profit_data) + (max(profit_data) * 0.2), label = paste0("$", formatC(max(profit_data), format="f", digits=0, big.mark=".", decimal.mark = ','))), size = 4) +
	#			theme_minimal()
	#	
	#	AUC(y_pred = predictions_prob_training, y_true = data_train$target)
	#	AUC(y_pred = predictions_prob_testing, y_true = data_test$target)
	#	LogLoss(y_pred = predictions_prob_training, y_true = data_train$target)
	#	LogLoss(y_pred = predictions_prob_testing, y_true = data_test$target)
	
	profit <- calculate_profit(x$cutoff, predictions_prob_testing, data_test$target)
		
	return(profit)  # devuelvo el negativo porque la libreria  mlrMBO   solo minimiza funciones
}






configureMlr(show.learner.output = FALSE)

#configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar

#https://github.com/catboost/benchmarks/blob/master/quality_benchmarks/comparison_description.pdf

objetive_function <- makeSingleObjectiveFunction(
		name = "catboost_optimizer",
		fn   = catboost_train,
		par.set = makeParamSet(
				makeIntegerParam("depth", lower = 1L, upper = 10L),
				#makeIntegerParam("iterations", lower = 250L, upper = 1500L),
				makeIntegerParam("iterations", lower = 250L, upper = 500L),
				makeIntegerParam("border_count", lower = 1L, upper = 254L), #subir fix para avisar que con 255 no va. Error in catboost.train(train_pool, NULL, fit_params) : c:/goagent/pipelines/buildmaster/catboost.git/catboost/cuda/gpu_data/compressed_index_builder.h:110: Error: can't proceed some features 
				makeIntegerParam("l2_leaf_reg", lower = 1, upper = 10),
				makeNumericParam("learning_rate", lower = 1e-07, upper = 1), #o 1e-06
				#makeNumericParam("rsm", lower = 0.01, upper = 1), #rsm on GPU is supported for pairwise modes only
				makeNumericParam("cutoff", lower = 0.01, upper = 0.1),
				makeIntegerParam("cutoff_in_logloss", lower = 0L, upper = 1L) #la regresion Kriging no soporta makeLogicalParam
				#random_strength ??? 1 a 20
				#bagging_temperature??? 0 1
		),
		minimize = FALSE,
		has.simple.signature = FALSE,
		global.opt.value = -1
)


mbo_iterations = 35

mbo_control <-  makeMBOControl(propose.points = 1L)
mbo_control <-  setMBOControlTermination(mbo_control, iters = mbo_iterations)
mbo_control <-  setMBOControlInfill(mbo_control, crit = makeMBOInfillCritEI(), opt  = "focussearch", opt.focussearch.points = mbo_iterations)

#design2 = generateDesign(n = 1, par.set = getParamSet(objetive_function))
#min_designs <- length(getParamSet(objetive_function)$pars) * 4
#
#initial_design <- expand.grid(
#		border_count = c(64, 128, 192),
#		cutoff = c(0.010, 0.015, 0.025, 0.035, 0.045, 0.055),
#		cutoff_in_logloss = c(0, 1),
#		depth = c(5, 6, 7),
#		iterations = 50,
#		l2_leaf_reg = c(1, 3, 5),
#		learning_rate = c(0.001, 0.03, 0.3)
#)
#
#design2 <- initial_design[sample(nrow(initial_design), min_designs), ]


#matern3_2
surrogate_learner <-  makeLearner("regr.km", predict.type = "se", covtype = "matern5_2", control = list(trace = FALSE))
mbo_learner  <- makeMBOLearner(mbo_control, objetive_function)
mbo_result  <-  mbo(objetive_function, learner = surrogate_learner, control = mbo_control, design = NULL)
plot(mbo_result)