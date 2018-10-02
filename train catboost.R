rm(list = ls())
gc()

library(catboost)
library(data.table)
library(dplyr)
library(MLmetrics)

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


#https://effectiveml.com/using-grid-search-to-optimise-catboost-parameters.html
fit_params <- list(iterations = 1000,
		random_seed = 666,
		#thread_count = 10,
		loss_function = 'Logloss:border=0.025',
		#loss_function = 'Logloss',
		#custom_loss = c('Logloss', 'AUC'),
		task_type = 'GPU',
		#ignored_features = c(4,9),
		#border_count = 32,
		depth = 6,
		#learning_rate = 0.02,
		#l2_leaf_reg = 3.5,
		train_dir = 'train_dir')

#model <- catboost.train(train_pool, test_pool, fit_params)
model <- catboost.train(train_pool, NULL, fit_params)


features_importance <- catboost.get_feature_importance(model, 
	pool = train_pool, 
	fstr_type = "FeatureImportance",
	thread_count = -1)
features_importance <- data.frame(feature = attr(features_importance,"names"), importance = features_importance)


predictions_prob_training <- catboost.predict(model, train_pool, prediction_type = 'Probability')
predictions_prob_testing <- catboost.predict(model, test_pool, prediction_type = 'Probability') 
#which(predictions > 0.025)

#profit
sum((predictions_prob_testing > 0.025) * ifelse(data_test$target == 1, 11000, -300))

AUC(y_pred = predictions_prob_training, y_true = data_train$target)
AUC(y_pred = predictions_prob_testing, y_true = data_test$target)

LogLoss(y_pred = predictions_prob_training, y_true = data_train$target)
LogLoss(y_pred = predictions_prob_testing, y_true = data_test$target)

