rm(list = ls())
invisible(gc())

library(catboost)
library(data.table)
library(dplyr)
library(Metrics)
library(ggplot2)

source('config.R')
source('functions.R')
#source('dataset_sql.R')

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

#remove_columns_final <- c('numero_de_cliente', 'foto_mes')
remove_columns_final <- c('foto_mes')

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

calculate_growth_polinomio <- F
calculate_acceleration_polinomio <- F
calculate_growth_dif_finitas <- F
calculate_acceleration_dif_finitas <- F
calculate_andreu_tendency <- F
calculate_derived <- F
calculate_max_min_etc <- F

remove_cols_with_more_than_20percent_NA <- F

#train_periods <- c(201802, 201801, 201712, 201711, 201710, 201709, 201708, 201707) #DEADLINE
#train_periods <- c(201802, 201801, 201712, 201704, 201703, 201702) #winner
#train_periods <- c(201802, 201801, 201712, 201704, 201702, 201701) #winner NUEVO??
#train_periods <- c(201802, 201801, 201712, 201702, 201701, 201612) #propuesta por viviana

train_periods <- c(201802)
#train_periods <- c(201801, 201712, 201711)
#train_periods <- c(201802, 201801, 201712, 201711)
#train_periods <- c(201802, 201801, 201712, 201711, 201710, 201709, 201708)
#train_periods <- c(201802, 201801, 201712)

#train_periods <- c(201705, 201704, 201703, 201702) #winner y testeo en 201804
#train_periods <- c(201704, 201703, 201702) #winner y testeo en 201804
#train_periods <- c(201704, 201703, 201802) #winner y testeo en 201804

#train_periods <- c(201607, 201606, 201605, 201604) #winner y testeo en 201706
#train_periods <- c(201606, 201605, 201604) #winner y testeo en 201706
#train_periods <- c(201704, 201703, 201702, 201606, 201604, 201603) #winner NUEVO??

#train_periods <- c(201711, 201712, 201801, 201703, 201702, 201701) #winner y testeo en 201803
#train_periods <- c(201812, 201702, 201701, 201712) #winner y testeo en 201802
#train_periods <- c(201612, 201611, 201610) #horrible y testeo en 201712
#train_periods <- c(201803, 201802, 201801, 201712, 201711, 201710) # y testear en 201804
############train_periods <- c(201802, 201801, 201712, 201711, 201710, 201709, 201708, 201707)  y teste en 201804, esta es la linea de muerte
########train_periods <- c(201704, 201703, 201702, 201701, 201612, 201611, 201610, 201609) es la linea de muerte

#data_train <- get_period(train_periods)
#data_train <- do.call(rbind, lapply(train_periods, function(period) fread(paste0(CONFIG$DATASETS_PATH, period, '_dias.txt'), header = TRUE, sep = "\t")))
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
data_train[, numero_de_cliente := NULL]


if (remove_cols_with_more_than_20percent_NA) {
	columns_na <- unname(which(colMeans(is.na(data_train)) > 0.2))
	data_train[, (columns_na) := NULL]
}

test_periods <- c(201806)
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
data_test_nro_cliente <- data_test$numero_de_cliente
data_test[, numero_de_cliente := NULL]



	if (remove_cols_with_more_than_20percent_NA) {
	data_test[, (columns_na) := NULL]
}

final_preprocess(data_train, TRUE)
final_preprocess(data_test, FALSE)

target_index <- c(which(names(data_train) == "target"))
categorical_indexes <- c(which(names(data_train) %in% categorical_features)) - 1

data_train <- as.data.frame(data_train)
#setDF(data_train)
train_pool <- catboost.load_pool(data = data_train[, - target_index], label = data_train[, target_index], cat_features = categorical_indexes)
#train_pool <- catboost.load_pool(data = setDF(data_train)[, -target_index], label = setDF(data_train, "target"))

data_test <- as.data.frame(data_test)
test_pool <- catboost.load_pool(data = data_test[, - target_index], label = data_test[, target_index], cat_features = categorical_indexes)

#for predict
#test_pool <- catboost.load_pool(data = data_test[, - target_index], cat_features = categorical_indexes)


#border <- round(sum(data_train$target) / nrow(data_train), 5)

fit_params <- list(
		loss_function = 'Logloss',
		#loss_function = 'Logloss:border=0.025',
		#loss_function = paste0('Logloss:border=', border),
		#custom_loss = c('Logloss', 'AUC'),
		task_type = 'GPU',
		train_dir = 'train_dir',
		logging_level = 'Verbose',

		iterations = 50

		#depth = 9,
		#border_count = 254,
		#learning_rate = 0.19656,
		#l2_leaf_reg = 21,
		#random_strength = 13,
		#bagging_temperature = 0
)

#model <- catboost.train(train_pool, test_pool, fit_params)
model <- catboost.train(train_pool, NULL, fit_params)
	
features_importance <- catboost.get_feature_importance(model, pool = train_pool, fstr_type = "FeatureImportance")
features_importance <- data.frame(feature = dimnames(train_pool)[[2]], importance = features_importance)
#features_importance <- data.frame(feature = attr(features_importance, "names"), importance = features_importance)
features_importance <- features_importance[order(features_importance$importance, decreasing = TRUE),]
	
predictions_prob_training <- catboost.predict(model, train_pool, prediction_type = 'Probability')
predictions_prob_testing <- catboost.predict(model, test_pool, prediction_type = 'Probability') 

invisible(gc())

df_prueba <- data.frame(prob = predictions_prob_testing, class = data_test$target, value = ifelse(data_test$target == 1, 11700, -300))
df_prueba <- df_prueba[order(df_prueba$prob, decreasing = TRUE),]
df_prueba$profit_acum <- cumsum(df_prueba$value)
#View(df_prueba[df_prueba$class == 1,])
df_prueba[df_prueba$profit_acum == max(df_prueba$profit_acum), ]

ggplot(df_prueba, aes(x = prob, y = profit_acum)) +
	geom_line() +
	coord_cartesian(ylim = c(0, max(df_prueba$profit_acum) * 1.05), xlim = c(0, df_prueba[df_prueba$profit_acum == max(df_prueba$profit_acum), "prob"] * 1.2)) +
	scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
	scale_x_continuous(labels = function(x) format(x, scientific = FALSE))


cutoffs <- seq(0, 0.1, by = 0.001)
#cutoffs <- seq(0, 0.05, by = 0.001)
	
profits <- sapply(cutoffs, calculate_profit, predictions_prob_testing, data_test$target)
profit_data <- data.frame(cutoff = cutoffs, profit = profits)

max_profit_cutoff <- min(profit_data[profit_data$profit == max(profit_data$profit),]$cutoff)
profit_default_cuttof <- profit_data[profit_data$cutoff == CONFIG$DEFAULT_CUTOFF,]$profit

ggplot(profit_data, aes(x = cutoff, y = profit)) +
		geom_line() +
		geom_vline(aes(xintercept = max_profit_cutoff), color = 'red', linetype = 2) +
		geom_vline(aes(xintercept = CONFIG$DEFAULT_CUTOFF), color = 'green', linetype = 2) +
		geom_hline(aes(yintercept = max(profit_data)), color = 'blue', linetype = 2) +
		scale_y_continuous("Profit", breaks = sort(c(seq(min(profit_data), max(profit_data), length.out = 6)))) +
		scale_x_continuous("Cutoff") +
		geom_text(aes(max_profit_cutoff, max(profit_data) - (max(profit_data) * 0.2), label = max_profit_cutoff), size = 4) +
		geom_text(aes(max_profit_cutoff, max(profit_data) + (max(profit_data) * 0.2), label = paste0("$", formatC(max(profit_data), format = "f", digits = 0, big.mark = ".", decimal.mark = ','))), size = 4) +
		geom_text(aes(CONFIG$DEFAULT_CUTOFF, 0, label = paste0("$", formatC(profit_default_cuttof, format = "f", digits = 0, big.mark = ".", decimal.mark = ','))), size = 4) +
		ylim(0, NA) +
		theme_minimal()

cat('Training:', train_periods, '\n')
cat('Testing:', test_periods, '\n')

cat('Max profit cutoff:', max_profit_cutoff, '\n')
cat('Max profit:', paste0("$", formatC(max(profit_data), format = "f", digits = 0, big.mark = ".", decimal.mark = ',')), '\n')
cat('Profit 0.025:', paste0("$", formatC(profit_default_cuttof, format = "f", digits = 0, big.mark = ".", decimal.mark = ',')), '\n')

auc_training <- round(auc(data_train$target, predictions_prob_training), 5)
auc_testing <- round(auc(data_test$target, predictions_prob_testing), 5)
logloss_training <- round(logLoss(data_train$target, predictions_prob_training), 5)
logloss_testing <- round(logLoss(data_test$target, predictions_prob_testing), 5)

cat('AUC training:', auc_training, '\n')
cat('AUC testing:', auc_testing, '\n')
cat('LogLoss training:', logloss_training, '\n')
cat('LogLoss testing:', logloss_testing, '\n')

perfect_profit_testing <- sum(data_test$target) * 11700
cat('Perfect profit ratio cutoff 0.025 testing:', round(profit_default_cuttof / perfect_profit_testing, 5), '\n')
cat('Perfect profit ratio cutoff', max_profit_cutoff, 'testing:', round(max(profit_data) / perfect_profit_testing, 5), '\n')