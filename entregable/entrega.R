library(catboost)
library(Metrics)

model_464 <- read.csv("464_predict_201804.csv")
model_529 <- read.csv("529_predict_201804.csv")
model_384 <- read.csv("384_predict_201804.csv")
model_137 <- read.csv("137_predict_201804.csv")
model_261 <- read.csv("261_predict_201804.csv")
model_000 <- read.csv("000_predict_201804.csv")

model_464 <- model_464[order(model_464$X),]
model_529 <- model_529[order(model_529$X),]
model_384 <- model_384[order(model_384$X),]
model_137 <- model_137[order(model_137$X),]
model_261 <- model_261[order(model_261$X),]
model_000 <- model_000[order(model_000$X),]

sum(model_464$X != model_529$X)
sum(model_464$X != model_384$X)
sum(model_464$X != model_137$X)
sum(model_464$X != model_261$X)
sum(model_464$X != model_000$X)

sum(model_464$class != model_529$class)
sum(model_464$class != model_384$class)
sum(model_464$class != model_137$class)
sum(model_464$class != model_261$class)
sum(model_464$class != model_000$class)

model_529$class <- NULL
model_384$class <- NULL
model_137$class <- NULL
model_261$class <- NULL
model_000$class <- NULL


stacktrain <- merge(x = model_464, y = model_529, by = "X", all = TRUE, suffixes = c("_model_464", "_model_529"))
stacktrain$prob_model_464_0 <- 1 - stacktrain$prob_model_464
stacktrain$prob_model_529_0 <- 1 - stacktrain$prob_model_529
stacktrain <- merge(x = stacktrain, y = model_384, by = "X", all = TRUE)
stacktrain$prob_model_384 <- stacktrain$prob
stacktrain$prob_model_384_0 <- 1 - stacktrain$prob
stacktrain$prob <- NULL
stacktrain <- merge(x = stacktrain, y = model_137, by = "X", all = TRUE)
stacktrain$prob_model_137 <- stacktrain$prob
stacktrain$prob_model_137_0 <- 1 - stacktrain$prob
stacktrain$prob <- NULL
stacktrain <- merge(x = stacktrain, y = model_261, by = "X", all = TRUE)
stacktrain$prob_model_261 <- stacktrain$prob
stacktrain$prob_model_261_0 <- 1 - stacktrain$prob
stacktrain$prob <- NULL
stacktrain <- merge(x = stacktrain, y = model_000, by = "X", all = TRUE)
stacktrain$prob_model_000 <- stacktrain$prob
stacktrain$prob_model_000_0 <- 1 - stacktrain$prob
stacktrain$prob <- NULL

stacktrain$prob_mean <- (stacktrain$prob_model_464 + stacktrain$prob_model_529 + stacktrain$prob_model_384 + stacktrain$prob_model_137 + stacktrain$prob_model_261 + stacktrain$prob_model_000) / 6
stacktrain$prob_mean_0 <- (stacktrain$prob_model_464_0 + stacktrain$prob_model_529_0 + stacktrain$prob_model_384_0 + stacktrain$prob_model_137_0 + stacktrain$prob_model_261_0 + stacktrain$prob_model_000_0) / 6

rownames(stacktrain) <- stacktrain$X
stacktrain$X <- NULL

stacktrain$class_tmp <- stacktrain$class
stacktrain$class <- NULL
stacktrain$class <- stacktrain$class_tmp
stacktrain$class_tmp <- NULL

class_index <- c(which(names(stacktrain) == "class"))

library(caret)

cvIndex <- createFolds(factor(stacktrain$class), 10, returnTrain = F)

train_logloss_values = c()
train_auc_values = c()
test_logloss_values = c()
test_auc_values = c()
test_profit_ratio_values = c()
test_profit_values = c()
train_profit_ratio_values = c()
train_profit_values = c()

perfect_profit <- sum(stacktrain$class) * 11700

for (i in 1:length(cvIndex)) {

	fold_train <- stacktrain[-cvIndex[[i]],]
	fold_test <- stacktrain[cvIndex[[i]],]

	perfect_profit_fold_train <- sum(fold_train$class) * 11700
	perfect_profit_fold <- sum(fold_test$class) * 11700

	pool_fold_train <- catboost.load_pool(data = fold_train[, - class_index], label = fold_train[, class_index])
	pool_fold_test <- catboost.load_pool(data = fold_test[, - class_index], label = fold_test[, class_index])

	df_pred_probs_training <- data.frame(matrix("", ncol = 0, nrow = nrow(fold_train)))
	df_pred_probs_testing <- data.frame(matrix("", ncol = 0, nrow = nrow(fold_test)))

	for (iter in 1:10) {
		#bagging de 10
		fit_params_cv <- list(
	  loss_function = 'Logloss',
	  task_type = 'GPU',
	  train_dir = 'train_dir',
	  logging_level = 'Silent',
	  iterations = 1100,
	  depth = 2,
	  border_count = 254,
	  random_seed = iter
	)

		print(paste0("Building model ", iter, " for fold ", i))
		model_cv <- catboost.train(pool_fold_train, NULL, fit_params_cv)

		df_pred_probs_training[paste0("prob_model_", iter)] <- catboost.predict(model_cv, pool_fold_train, prediction_type = 'Probability')
		df_pred_probs_testing[paste0("prob_model_", iter)] <- catboost.predict(model_cv, pool_fold_test, prediction_type = 'Probability')

		invisible(gc())
	}

	pred_prob_fold_train <- rowMeans(df_pred_probs_training)
	pred_prob_fold_test <- rowMeans(df_pred_probs_testing)

	auc_training <- auc(fold_train$class, pred_prob_fold_train)
	logloss_training <- logLoss(fold_train$class, pred_prob_fold_train)

	auc_testing <- auc(fold_test$class, pred_prob_fold_test)
	logloss_testing <- logLoss(fold_test$class, pred_prob_fold_test)

	df_profit_calc_train <- data.frame(prob = pred_prob_fold_train, class = fold_train$class, value = ifelse(fold_train$class == 1, 11700, -300))
	df_profit_calc_train <- df_profit_calc_train[order(df_profit_calc_train$prob, decreasing = TRUE),]
	df_profit_calc_train$profit_acum <- cumsum(df_profit_calc_train$value)
	row_max_profit_train <- head(df_profit_calc_train[df_profit_calc_train$profit_acum == max(df_profit_calc_train$profit_acum),], 1)

	train_profit_ratio_values = c(train_profit_ratio_values, row_max_profit_train$profit_acum / perfect_profit_fold_train)
	train_profit_values = c(train_profit_values, (row_max_profit_train$profit_acum / perfect_profit_fold_train) * perfect_profit)

	df_profit_calc <- data.frame(prob = pred_prob_fold_test, class = fold_test$class, value = ifelse(fold_test$class == 1, 11700, -300))
	df_profit_calc <- df_profit_calc[order(df_profit_calc$prob, decreasing = TRUE),]
	df_profit_calc$profit_acum <- cumsum(df_profit_calc$value)
	row_max_profit <- head(df_profit_calc[df_profit_calc$profit_acum == max(df_profit_calc$profit_acum),], 1)

	test_profit_ratio_values = c(test_profit_ratio_values, row_max_profit$profit_acum / perfect_profit_fold)
	test_profit_values = c(test_profit_values, (row_max_profit$profit_acum / perfect_profit_fold) * perfect_profit)

	train_logloss_values = c(train_logloss_values, logloss_training)
	train_auc_values = c(train_auc_values, auc_training)
	test_logloss_values = c(test_logloss_values, logloss_testing)
	test_auc_values = c(test_auc_values, auc_testing)

}

cat('Mean AUC training:', round(mean(train_auc_values), 5), '\n')
cat('Mean Log Loss training:', round(mean(train_logloss_values), 5), '\n')
cat('Mean Perfect profit ratio training:', round(mean(train_profit_ratio_values), 2), '\n')
cat('Mean Profit ratio training:', round(mean(train_profit_values), 0), '\n')

cat('Mean AUC testing:', round(mean(test_auc_values), 5), '\n')
cat('Mean LogLoss testing:', round(mean(test_logloss_values), 5), '\n')
cat('Mean Perfect profit ratio testing:', round(mean(test_profit_ratio_values), 2), '\n')
cat('Mean Profit ratio testing:', round(mean(test_profit_values), 0), '\n')


table(stacktrain$class)
table(stacktrain[cvIndex[[1]], "class"])
table(stacktrain[cvIndex[[2]], "class"])
table(stacktrain[cvIndex[[3]], "class"])
table(stacktrain[cvIndex[[4]], "class"])
table(stacktrain[cvIndex[[5]], "class"])


catboos_cv <- function() {

}




pool <- catboost.load_pool(data = stacktrain[, - class_index], label = stacktrain[, class_index])

stacked_models <- list()

for (iter in 1:10) {
	fit_params <- list(
	loss_function = 'Logloss',
	task_type = 'GPU',
	train_dir = 'train_dir',
	logging_level = 'Silent',

	iterations = 1100,
	random_seed = iter,

	depth = 2
	#border_count = 254,
	#learning_rate = 0.19656,
	#l2_leaf_reg = 21,
	#random_strength = 26,
	#bagging_temperature = 1

    )

	stacked_models[[length(stacked_models) + 1]] <- catboost.train(pool, NULL, fit_params)
}


df_pred_probs <- data.frame(matrix("", ncol = 0, nrow = nrow(stacktrain)))
for (i in 1:length(stacked_models)) {
	df_pred_probs[paste0("prob_model_", i)] <- catboost.predict(stacked_models[[i]], pool, prediction_type = 'Probability')
}
df_pred_probs$prob_prom <- rowMeans(df_pred_probs)


df_prueba <- data.frame(prob = df_pred_probs$prob_prom, class = stacktrain$class, value = ifelse(stacktrain$class == 1, 11700, -300))
df_prueba <- df_prueba[order(df_prueba$prob, decreasing = TRUE),]
df_prueba$profit_acum <- cumsum(df_prueba$value)
df_prueba[df_prueba$profit_acum == max(df_prueba$profit_acum),]
nrow(df_prueba[df_prueba$prob >= df_prueba[df_prueba$profit_acum == max(df_prueba$profit_acum),]$prob,])
nrow(df_prueba[df_prueba$prob >= df_prueba[df_prueba$profit_acum == max(df_prueba$profit_acum),]$prob & df_prueba$class == 1,])
nrow(df_prueba[df_prueba$prob >= df_prueba[df_prueba$profit_acum == max(df_prueba$profit_acum),]$prob & df_prueba$class == 0,])


df_tmp <- df_prueba[df_prueba$profit_acum >= 7490000 & df_prueba$profit_acum <= 7510000,]
df_tmp <- df_tmp[order(df_tmp$profit_acum, decreasing = F),]
mean(df_tmp$prob) #punto de corte 0.025


df_tmp2 <- df_prueba[df_prueba$prob >= 0.025,]
df_tmp2 <- df_tmp2[order(df_tmp2$prob, decreasing = F),]
mean(df_tmp$prob)





auc_training <- round(auc(stacktrain$class, predictions_prob_training), 5)
logloss_training <- round(logLoss(stacktrain$class, predictions_prob_training), 5)

cat('AUC training:', auc_training, '\n')
cat('LogLoss training:', logloss_training, '\n')



df_mean_prob <- data.frame(prob = rowMeans(stacktrain[, - class_index]), class = stacktrain$class, value = ifelse(stacktrain$class == 1, 11700, -300))
df_mean_prob <- df_mean_prob[order(df_mean_prob$prob, decreasing = TRUE),]
df_mean_prob$profit_acum <- cumsum(df_mean_prob$value)
df_mean_prob[df_mean_prob$profit_acum == max(df_mean_prob$profit_acum),]
