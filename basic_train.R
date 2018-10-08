rm(list = ls())
invisible(gc())

library(catboost)
library(data.table)
library(dplyr)
library(Metrics)
library(ggplot2)


datasets_location <- 'D:\\maestriadm\\dm economia finanzas\\bankchurn\\dias\\'

memory.limit(8192)

DEFAULT_CUTOFF <- 0.025

train_periods <- c(201802)
#train_periods <- c(201801, 201712, 201711)
#train_periods <- c(201802, 201801, 201712, 201711, 201710, 201709, 201708)
#train_periods <- c(201802, 201801, 201712)


data_train <- do.call(rbind, lapply(train_periods, function(period) fread(paste0(datasets_location, period,'_dias.txt'), header = TRUE, sep = "\t")))
data_train <- data_train[sample(1:nrow(data_train)), ] 

#glimpse(data_train)

data_test <- fread(paste0(datasets_location, '201804_dias.txt'), header = TRUE, sep = "\t")

#data_structure <- data.frame(class = sapply(data_train, class))
#data_structure$field <- rownames(data_structure)
#rownames(data_structure) <- NULL
#
#data_structure %>% 
#	group_by(class) %>% 
#	summarise(n = n())

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



#downsampling
#cantbajas <- round(nrow(data_train[data_train$clase_ternaria != 'CONTINUA',]) * 1.50)
#rowsContinua <- sample(which(data_train$clase_ternaria == 'CONTINUA'), size = cantbajas)
#rowsBaja <- which(data_train$clase_ternaria != 'CONTINUA')
#data_train <- data_train[c(rowsContinua,rowsBaja), ]
#data_train <- data_train[sample(1:nrow(data_train)), ]

#oversampling
#cantbajas <- round(nrow(data_train[data_train$clase_ternaria == 'BAJA+2',]) * 50)
#rowsBaja <- which(data_train$clase_ternaria == 'BAJA+2')
#rowsBaja <- sample(rowsBaja, size = cantbajas, replace = TRUE)
#data_train <- rbind(data_train, data_train[rowsBaja, ])
#data_train <- data_train[sample(1:nrow(data_train)), ]


#smote con rose
#library(ROSE)
#data.rose <- ROSE(clase_ternaria ~ ., data = data_train[data_train$clase_ternaria != 'BAJA+1',], N = 50000, p = 0.99, seed = 1)$data
#data_train <- rbind(data_train, data.rose[data.rose$clase_ternaria == 'BAJA+2',]) 
#probar con library(smotefamily)


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

	
fit_params <- list(
		iterations = 1000,
		loss_function = 'Logloss',
		#loss_function = 'Logloss:border=0.025',
		#custom_loss = c('Logloss', 'AUC'),
		task_type = 'GPU',
		#ignored_features = c(4,9),
		#border_count = 128,
		#depth = 6,
		train_dir = 'train_dir',
		logging_level = 'Verbose'		
)

#model <- catboost.train(train_pool, test_pool, fit_params)
model <- catboost.train(train_pool, NULL, fit_params)

	
#features_importance <- catboost.get_feature_importance(model, 
#		pool = train_pool, 
#		fstr_type = "FeatureImportance",
#		thread_count = -1)
#features_importance <- data.frame(feature = attr(features_importance,"names"), importance = features_importance)

	
predictions_prob_training <- catboost.predict(model, train_pool, prediction_type = 'Probability')
predictions_prob_testing <- catboost.predict(model, test_pool, prediction_type = 'Probability') 
	
invisible(gc())
	
cutoffs <- seq(0, 0.1, by = 0.001)
#cutoffs <- seq(0, 0.05, by = 0.001)
	
profits <- sapply(cutoffs, calculate_profit, predictions_prob_testing, data_test$target)
profit_data <- data.frame(cutoff = cutoffs, profit = profits)

max_profit_cutoff <- profit_data[profit_data$profit == max(profit_data$profit), ]$cutoff

ggplot(profit_data, aes(x = cutoff, y = profit)) +
		geom_line() +
		geom_vline(aes(xintercept = max_profit_cutoff), color='red', linetype=2) + 
		geom_vline(aes(xintercept = DEFAULT_CUTOFF), color='green', linetype=2) +
		geom_hline(aes(yintercept = max(profit_data)), color='blue', linetype=2) +
		scale_y_continuous("Profit", breaks = sort(c(seq(min(profit_data), max(profit_data), length.out=6)))) +
		scale_x_continuous("Cutoff") +
		geom_text(aes(max_profit_cutoff, max(profit_data) - (max(profit_data) * 0.2), label = max_profit_cutoff), size = 4) +
		geom_text(aes(max_profit_cutoff, max(profit_data) + (max(profit_data) * 0.2), label = paste0("$", formatC(max(profit_data), format="f", digits=0, big.mark=".", decimal.mark = ','))), size = 4) +
		geom_text(aes(DEFAULT_CUTOFF, 0, label = paste0("$", formatC(profit_data[profit_data$cutoff == DEFAULT_CUTOFF, ]$profit, format="f", digits=0, big.mark=".", decimal.mark = ','))), size = 4) +
		theme_minimal()

auc(data_train$target, predictions_prob_training)
auc(data_test$target, predictions_prob_testing)
logLoss(data_train$target, predictions_prob_training)
logLoss(data_test$target, predictions_prob_testing)
