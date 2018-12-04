library(catboost)
library(Metrics)

data_model_384 <- read.csv("384_predict_201806.csv")
data_model_261 <- read.csv("261_predict_201806.csv")
data_model_464 <- read.csv("464_predict_201806.csv")
data_model_529 <- read.csv("529_predict_201806.csv")
data_model_137 <- read.csv("137_predict_201806.csv")
data_model_000 <- read.csv("000_predict_201806.csv")

data_model_384 <- data_model_384[order(data_model_384$X),]
data_model_261 <- data_model_261[order(data_model_261$X),]
data_model_464 <- data_model_464[order(data_model_464$X),]
data_model_529 <- data_model_529[order(data_model_529$X),]
data_model_137 <- data_model_137[order(data_model_137$X),]
data_model_000 <- data_model_000[order(data_model_000$X),]

sum(data_model_384$X != data_model_261$X)
sum(data_model_384$X != data_model_464$X)
sum(data_model_384$X != data_model_529$X)
sum(data_model_384$X != data_model_137$X)
sum(data_model_384$X != data_model_000$X)




df_predict <- merge(x = data_model_464, y = data_model_529, by = "X", all = TRUE, suffixes = c("_model_464", "_model_529"))
df_predict2 <- merge(x = df_predict, y = data_model_384, by = "X", all = TRUE)
df_predict2$prob_model_384 <- df_predict2$prob
df_predict2$prob <- NULL
df_predict3 <- merge(x = df_predict2, y = data_model_137, by = "X", all = TRUE)
df_predict3$prob_model_137 <- df_predict3$prob
df_predict3$prob <- NULL
df_predict4 <- merge(x = df_predict3, y = data_model_261, by = "X", all = TRUE)
df_predict4$prob_model_261 <- df_predict4$prob
df_predict4$prob <- NULL
df_predict5 <- merge(x = df_predict4, y = data_model_000, by = "X", all = TRUE)
df_predict5$prob_model_000 <- df_predict5$prob
df_predict5$prob <- NULL


rownames(df_predict5) <- df_predict5$X
df_predict5$X <- NULL

pool_predict <- catboost.load_pool(data = df_predict5)

predictions_prob_201806 <- catboost.predict(model_stacked, pool_predict, prediction_type = 'Probability')

data_entrega = data.frame(numero_de_cliente = rownames(df_predict5), prob = predictions_prob_201806)
data_entrega <- data_entrega[order(data_entrega$prob, decreasing = TRUE),]