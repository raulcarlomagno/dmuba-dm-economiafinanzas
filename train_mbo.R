rm(list = ls())
invisible(gc())

if (Sys.info()[['sysname']] == 'Linux'){
	setwd('/home/rcarlomagno/catboost/')
}else{
	setwd('D:\\maestriadm\\dm economia finanzas\\bankchurn\\')
}

library(catboost)
library(mlrMBO)
library(Metrics)

EXPERIMENT_NAME <- 'solo andreu 201802, 201801, 201712, 201704, 201702, 201701'
PLAN_ID <- 682

OPTIMIZATION_ITERATIONS <- 200
SAVE_PROGRESS_EACH <- 600 #en segundos
PROGRESS_FILENAME <-paste0('mbo_progress_exp_', PLAN_ID,'.RData')

source('config.R')
source('functions.R')
source('results_sql.R')

remove_columns <- c(
	 'tpaquete1',
	 'tpaquete2',
	 'tpaquete3',
	 'tpaquete4',
	 'tpaquete5',
	 'tpaquete6',
	 'tpaquete8'
#'mcuenta_corriente_dolares',
#'tfondos_comunes_inversion',
#'mbonos_corporativos',
#'mmonedas_extranjeras',
#'minversiones_otras',
#'ccuenta_descuentos',
#'tautoservicio',
#'cautoservicio_transacciones',
#'Master_marca_atraso',
#'Visa_madelantodolares'
)

remove_columns_final <- c('numero_de_cliente', 'foto_mes')

avoid_tendency_columns <- c(
	"numero_de_cliente",
	"foto_mes",
	"marketing_activo_ultimos90dias",
	"cliente_vip",
	"internet",
	"cliente_edad",
	"cliente_antiguedad",
	"tpaquete7",
	"tpaquete9",
	"tcuenta_corriente",
	"tcaja_seguridad",
	"tcallcenter",
	"thomebanking",
	"Master_marca_atraso",
	"Master_cuenta_estado",
	"Master_Fvencimiento",
	"Master_Finiciomora",
	"Master_fultimo_cierre",
	"Master_fechaalta",
	"Visa_marca_atraso",
	"Visa_cuenta_estado",
	"Visa_Fvencimiento",
	"Visa_Finiciomora",
   "Visa_fultimo_cierre",
   "Visa_fechaalta",
   "clase_ternaria",
	"antig_ratio"
)

categorical_features <- c("cliente_edad", "Master_cuenta_estado", "Visa_cuenta_estado")




calculate_growth_polinomio <- T
calculate_acceleration_polinomio <- T
calculate_growth_dif_finitas <- T
calculate_acceleration_dif_finitas <- T
calculate_andreu_tendency <- T
calculate_derived <- T
calculate_max_min_etc <- F





#train_periods <- c(201802)
#train_periods <- c(201802, 201801, 201712, 201704, 201702, 201701)
train_periods <- c(201704, 201703, 201701, 201612, 201611, 201606, 201605, 201604)

data_train <- load_dataset(
		train_periods,
		remove_columns,
		avoid_tendency_columns,
		categorical_features,
		remove_columns_final,
		growth_polinomio = calculate_growth_polinomio,
		acceleration_polinomio = calculate_acceleration_polinomio,
		growth_dif_finitas = calculate_growth_dif_finitas,
		acceleration_dif_finitas = calculate_acceleration_dif_finitas,
		tendency_andreu = calculate_andreu_tendency,
		derived = calculate_derived,
		max_min_etc = calculate_max_min_etc
)



#test_periods <- c(201804)
test_periods <- c(201706)
data_test <- load_dataset(
		test_periods,
		remove_columns,
		avoid_tendency_columns,
		categorical_features,
		remove_columns_final,
		growth_polinomio = calculate_growth_polinomio,
		acceleration_polinomio = calculate_acceleration_polinomio,
		growth_dif_finitas = calculate_growth_dif_finitas,
		acceleration_dif_finitas = calculate_acceleration_dif_finitas,
		tendency_andreu = calculate_andreu_tendency,
		derived = calculate_derived,
		max_min_etc = calculate_max_min_etc
)

final_preprocess(data_train, TRUE)
final_preprocess(data_test, FALSE)

target_index <- c(which(names(data_train) == "target"))
categorical_indexes <- c(which(names(data_train) %in% categorical_features)) - 1

data_train <- as.data.frame(data_train)
train_pool <- catboost.load_pool(data = data_train[, - target_index], label = data_train[, target_index], cat_features = categorical_indexes)

data_test <- as.data.frame(data_test)
test_pool <- catboost.load_pool(data = data_test[, - target_index], label = data_test[, target_index], cat_features = categorical_indexes)


catboost_train <- function(x = list(
				depth,
				iterations,
				l2_leaf_reg,
				learning_rate,
				random_strength,
				bagging_temperature
)) {


	#https://effectiveml.com/using-grid-search-to-optimise-catboost-parameters.html
	fit_params <- list(
			iterations = x$iterations,
			loss_function = 'Logloss',
			task_type = 'GPU',
			border_count = 254, #try to set the value of this parameter to 254 when training on GPU if the best possible quality is required.
			depth = x$depth,
			learning_rate = x$learning_rate,
			l2_leaf_reg = x$l2_leaf_reg,
			train_dir = CONFIG$TRAIN_DIR,
			logging_level = 'Verbose',
			#logging_level = 'Silent',
			random_strength = x$random_strength,
			bagging_temperature = x$bagging_temperature
	)

	minutes_taken_total <- c()
	profit_total <- c() 
	profit_default_cutoff_total <- c()
	auc_testing_total <- c()
	logloss_testing_total <- c()
	auc_training_total <- c()
	logloss_training_total <- c()
	profit_ratio_cutoff_total <- c()
	profit_ratio_default_cutoff_total <- c()
	max_profit_cutoff_total <- c()


	for (iteration in 1:3) {

		cat('Iteration', iteration, '\n')
		cat("training with these hyperparameters:\n")
		print(data.frame(x), row.names = FALSE)


		train_start_time <- Sys.time()
		model <- catboost.train(train_pool, NULL, fit_params)
		train_end_time <- Sys.time()

		predictions_prob_training <- catboost.predict(model, train_pool, prediction_type = 'Probability')
		predictions_prob_testing <- catboost.predict(model, test_pool, prediction_type = 'Probability')

		invisible(gc())

		cutoffs <- seq(0, 0.1, by = 0.001)
		profits <- sapply(cutoffs, calculate_profit, predictions_prob_testing, data_test$target)
		profit_data <- data.frame(cutoff = cutoffs, profit = profits)
		max_profit_cutoff <- max(profit_data[profit_data$profit == max(profit_data$profit),]$cutoff)
		profit_default_cutoff <- (profit_data[profit_data$cutoff == CONFIG$DEFAULT_CUTOFF,]$profit)


		profit <- max(profit_data$profit)
		perfect_profit <- sum(data_test$target) * 11700

		profit_ratio_cutoff <- round(profit / perfect_profit, 5)


		auc_training <- round(auc(data_train$target, predictions_prob_training), 5)
		auc_testing <- round(auc(data_test$target, predictions_prob_testing), 5)
		logloss_training <- round(logLoss(data_train$target, predictions_prob_training), 5)
		logloss_testing <- round(logLoss(data_test$target, predictions_prob_testing), 5)


		cat('Perfect profit:', format_money(perfect_profit), '\n')
		cat('Max profit cutoff:', max_profit_cutoff, '\n')
		cat('Max profit:', format_money(profit), '\n')
		cat('Profit 0.025:', format_money(profit_default_cutoff), '\n')


		cat('Perfect profit ratio cutoff 0.025:', round(profit_default_cutoff / perfect_profit, 5), '\n')
		cat('Perfect profit ratio cutoff', max_profit_cutoff, ':', round(max(profit_data$profit) / perfect_profit, 5), '\n')

		cat('AUC training:', auc_training, '\n')
		cat('AUC testing:', auc_testing, '\n')
		cat('LogLoss training:', logloss_training, '\n')
		cat('LogLoss testing:', logloss_testing, '\n')


		minutes_taken_total <- append(minutes_taken_total, round(as.numeric(train_end_time - train_start_time, units = "mins"), 2))
		profit_total <- append(profit_total, profit)
		profit_default_cutoff_total <- append(profit_default_cutoff_total, profit_default_cutoff)
		auc_testing_total <- append(auc_testing_total, auc_testing)
		logloss_testing_total <- append(logloss_testing_total, logloss_testing)
		auc_training_total <- append(auc_training_total, auc_training)
		logloss_training_total <- append(logloss_training_total, logloss_training)
		profit_ratio_cutoff_total <- append(profit_ratio_cutoff_total, profit_ratio_cutoff)
		profit_ratio_default_cutoff_total <- append(profit_ratio_default_cutoff_total, round(profit_default_cutoff / perfect_profit, 5))
		max_profit_cutoff_total <- append(max_profit_cutoff_total, max_profit_cutoff)

	}


	insert_experiment_result(
			EXPERIMENT_NAME,
			round(sum(minutes_taken_total), 2),
			mean(profit_total),
			mean(profit_default_cutoff_total),
			round(mean(auc_testing_total), 5),
			round(mean(logloss_testing_total), 5),
			round(mean(auc_training_total), 5),
			round(mean(logloss_training_total), 5),
			round(profit_ratio_cutoff_total, 5),
			round(profit_ratio_default_cutoff_total, 5),
			round(mean(max_profit_cutoff_total), 5),
			'catboost',
			'cv montecarlo 3x',
			paste(train_periods, collapse = ', '),
			paste(test_periods, collapse = ', '),
			'',
			PLAN_ID,
			fit_params
		)

	save_mbo_evo_plot(PLAN_ID)

	return(mean(profit_total))
}


configureMlr(show.learner.output = FALSE)

#configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
#https://github.com/catboost/benchmarks/blob/master/quality_benchmarks/comparison_description.pdf
objetive_function <- makeSingleObjectiveFunction(
		name = "catboost_optimizer",
		fn   = catboost_train,
		par.set = makeParamSet(
				makeIntegerParam("depth", lower = 4L, upper = 10L, default = 6L),
				makeIntegerParam("iterations", lower = 500L, upper = 1250L, default = 1000L),
				#makeIntegerParam("iterations", lower = 1L, upper = 5L),
				makeIntegerParam("l2_leaf_reg", lower = 1, upper = 20, default = 3), #upper = 10
				makeNumericParam("learning_rate", lower = 1e-07, upper = 1, default = 0.03), #o 1e-06
				makeNumericParam("random_strength", lower = 1, upper = 20, default = 1),
				makeNumericParam("bagging_temperature", lower = 0, upper = 20, default = 1) #upper = 1
		),
		minimize = FALSE,
		has.simple.signature = FALSE,
		global.opt.value = -1
)


mbo_control <- makeMBOControl(propose.points = 1L, save.on.disk.at.time = SAVE_PROGRESS_EACH, save.file.path = PROGRESS_FILENAME)
mbo_control <- setMBOControlTermination(mbo_control, iters = OPTIMIZATION_ITERATIONS)
mbo_control <- setMBOControlInfill(mbo_control, crit = makeMBOInfillCritEI(), opt = "focussearch", opt.focussearch.points = OPTIMIZATION_ITERATIONS)

#matern3_2
surrogate_learner <-  makeLearner("regr.km", predict.type = "se", covtype = "matern5_2", control = list(trace = FALSE))
mbo_learner  <- makeMBOLearner(mbo_control, objetive_function)


mbo_start_time <- Sys.time()
if (file.exists(PROGRESS_FILENAME)) {
	mboContinue(PROGRESS_FILENAME)
} else {
	mbo_result <- mbo(objetive_function, learner = surrogate_learner, control = mbo_control, design = NULL)
}
mbo_end_time <- Sys.time()

mbo_end_time - mbo_start_time

cat("best hyperparameters:\n")
print(data.frame(mbo_result$x), row.names = FALSE )

print(paste0("Max optimized profit: ", format_money(mbo_result$y)))

png(paste0(CONFIG$WORK_PATH, "mbo_", PLAN_ID, ".png "), width = 1920, height = 1080)
plot(mbo_result)
dev.off()