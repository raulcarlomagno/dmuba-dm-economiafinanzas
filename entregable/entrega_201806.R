library(catboost)
library(Metrics)

data_model_384 <- read.csv("384_predict_201806.csv")
data_model_261 <- read.csv("261_predict_201806.csv")

data_model_384 <- data_model_384[order(data_model_384$X),]
data_model_261 <- data_model_261[order(data_model_261$X),]

sum(data_model_384$X != data_model_261$X)

df_stacked <- merge(x = data_model_384, y = data_model_261, by = "X", all = TRUE, suffixes = c("_model_384", "_model_261"))
stacked2 <- merge(x = stacked, y = model_384, by = "X", all = TRUE)
stacked2$prob_model_384 <- stacked2$prob
stacked2$prob <- NULL
stacked3 <- merge(x = stacked2, y = model_137, by = "X", all = TRUE)
stacked3$prob_model_137 <- stacked3$prob
stacked3$prob <- NULL
stacked4 <- merge(x = stacked3, y = model_261, by = "X", all = TRUE)
stacked4$prob_model_261 <- stacked4$prob
stacked4$prob <- NULL


rownames(stacked4) <- stacked4$X
stacked4$X <- NULL

class_index <- c(which(names(stacked4) == "class"))
pool <- catboost.load_pool(data = stacked4[, - class_index], label = stacked4[, class_index])

fit_params <- list(
		loss_function = 'Logloss',
		task_type = 'GPU',
		train_dir = 'train_dir',
		logging_level = 'Verbose',

		iterations = 1000

		#depth = 9,
		#border_count = 254,
		#learning_rate = 0.19656,
		#l2_leaf_reg = 21,
		#random_strength = 13,
		#bagging_temperature = 0
)

model_stacked <- catboost.train(pool, NULL, fit_params)

predictions_prob_training <- catboost.predict(model_stacked, pool, prediction_type = 'Probability')

#df <- data.frame(prom = rowMeans(stacked4[, -2]), prob = predictions_prob_training)
#df$diff <- (df$prob - df$prom)
#sum(df$diff)

df_prueba <- data.frame(prob = predictions_prob_training, class = stacked4$class, value = ifelse(stacked4$class == 1, 11700, -300))
df_prueba <- df_prueba[order(df_prueba$prob, decreasing = TRUE),]
df_prueba$profit_acum <- cumsum(df_prueba$value)
df_prueba[df_prueba$profit_acum == max(df_prueba$profit_acum),]

auc_training <- round(auc(stacked4$class, predictions_prob_training), 5)
logloss_training <- round(logLoss(stacked4$class, predictions_prob_training), 5)

cat('AUC training:', auc_training, '\n')
cat('LogLoss training:', logloss_training, '\n')
