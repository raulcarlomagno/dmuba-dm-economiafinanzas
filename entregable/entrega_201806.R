library(catboost)
library(Metrics)


data_model_464_201806 <- read.csv("464_predict_201806.csv")
data_model_529_201806 <- read.csv("529_predict_201806.csv")
data_model_384_201806 <- read.csv("384_predict_201806.csv")
data_model_137_201806 <- read.csv("137_predict_201806.csv")
data_model_261_201806 <- read.csv("261_predict_201806.csv")
data_model_000_201806 <- read.csv("000_predict_201806.csv")

data_model_464_201806 <- data_model_464_201806[order(data_model_464_201806$X),]
data_model_529_201806 <- data_model_529_201806[order(data_model_529_201806$X),]
data_model_384_201806 <- data_model_384_201806[order(data_model_384_201806$X),]
data_model_137_201806 <- data_model_137_201806[order(data_model_137_201806$X),]
data_model_261_201806 <- data_model_261_201806[order(data_model_261_201806$X),]
data_model_000_201806 <- data_model_000_201806[order(data_model_000_201806$X),]

sum(data_model_464_201806$X != data_model_529_201806$X)
sum(data_model_464_201806$X != data_model_384_201806$X)
sum(data_model_464_201806$X != data_model_137_201806$X)
sum(data_model_464_201806$X != data_model_261_201806$X)
sum(data_model_464_201806$X != data_model_000_201806$X)


df_predict <- merge(x = data_model_464_201806, y = data_model_529_201806, by = "X", all = TRUE, suffixes = c("_model_464", "_model_529"))
df_predict <- merge(x = df_predict, y = data_model_384_201806, by = "X", all = TRUE)
df_predict$prob_model_384 <- df_predict$prob
df_predict$prob <- NULL
df_predict <- merge(x = df_predict, y = data_model_137_201806, by = "X", all = TRUE)
df_predict$prob_model_137 <- df_predict$prob
df_predict$prob <- NULL
df_predict <- merge(x = df_predict, y = data_model_261_201806, by = "X", all = TRUE)
df_predict$prob_model_261 <- df_predict$prob
df_predict$prob <- NULL
df_predict <- merge(x = df_predict, y = data_model_000_201806, by = "X", all = TRUE)
df_predict$prob_model_000 <- df_predict$prob
df_predict$prob <- NULL

rownames(df_predict) <- df_predict$X
df_predict$X <- NULL

pool_predict <- catboost.load_pool(data = df_predict)

df_pred_probs_201806 <- data.frame(matrix("", ncol = 0, nrow = nrow(df_predict)))
for (i in 1:length(stacked_models)) {
	df_pred_probs_201806[paste0("prob_model_", i)] <- catboost.predict(stacked_models[[i]], pool_predict, prediction_type = 'Probability')
}
df_pred_probs_201806$prob_prom <- rowMeans(df_pred_probs_201806)

data_entrega = data.frame(numero_de_cliente = rownames(df_predict), prob = df_pred_probs_201806$prob_prom)
data_entrega <- data_entrega[order(data_entrega$prob, decreasing = TRUE),]

write.table(data_entrega[1:7664, "numero_de_cliente"], "diaz_carlomagno.txt", row.names = F, quote = F, col.names = F)