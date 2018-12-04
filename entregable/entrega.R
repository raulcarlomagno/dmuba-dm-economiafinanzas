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


stacked <- merge(x = model_464, y = model_529, by = "X", all = TRUE, suffixes = c("_model_464", "_model_529"))
stacked2 <- merge(x = stacked, y = model_384, by = "X", all = TRUE)
stacked2$prob_model_384 <- stacked2$prob
stacked2$prob <- NULL
stacked3 <- merge(x = stacked2, y = model_137, by = "X", all = TRUE)
stacked3$prob_model_137 <- stacked3$prob
stacked3$prob <- NULL
stacked4 <- merge(x = stacked3, y = model_261, by = "X", all = TRUE)
stacked4$prob_model_261 <- stacked4$prob
stacked4$prob <- NULL
stacked5 <- merge(x = stacked4, y = model_000, by = "X", all = TRUE)
stacked5$prob_model_000 <- stacked5$prob
stacked5$prob <- NULL


rownames(stacked5) <- stacked5$X
stacked5$X <- NULL

class_index <- c(which(names(stacked5) == "class"))
pool <- catboost.load_pool(data = stacked5[, - class_index], label = stacked5[, class_index])

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
nrow(df_prueba[df_prueba$prob >= df_prueba[df_prueba$profit_acum == max(df_prueba$profit_acum),]$prob,])
nrow(df_prueba[df_prueba$prob >= df_prueba[df_prueba$profit_acum == max(df_prueba$profit_acum),]$prob & df_prueba$class == 1,])
nrow(df_prueba[df_prueba$prob >= df_prueba[df_prueba$profit_acum == max(df_prueba$profit_acum),]$prob & df_prueba$class == 0,])

auc_training <- round(auc(stacked4$class, predictions_prob_training), 5)
logloss_training <- round(logLoss(stacked4$class, predictions_prob_training), 5)

cat('AUC training:', auc_training, '\n')
cat('LogLoss training:', logloss_training, '\n')
