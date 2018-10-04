library(data.table)
library(ggplot2)
library(dplyr)
library(DataExplorer)

library(rpart)
library(party)


dirbase <- 'D:\\Dropbox\\UBA2018\\'
lockBinding("dirbase", globalenv()) #constante


data <- fread(paste0(dirbase, 'datasets/201804.txt'), encoding = "UTF-8", stringsAsFactors = T)
data2 <- fread(paste0(dirbase, 'datasets/201802.txt'), encoding = "UTF-8", stringsAsFactors = T)

plot_missing(data)
glimpse(data)

table(data$clase_ternaria)/nrow(data) * 100
table(data2$clase_ternaria)/nrow(data) * 100

ggplot(data, aes(x = clase_ternaria, y = ccheques_emitidos)) +
		#geom_boxplot(outlier.shape = NA)
		geom_boxplot()


#train_df <- sim_data(n = 1000,
#		modelvars = 30,
#		noisevars = 2000,
#		model_sd = 4,
#		noise_sd = 4,
#		epsilon_sd = 4,
#		outcome = "regression",
#		cutoff = NULL)


#data[, clase_ternaria := ifelse(clase_ternaria  == "CONTINUA",0 ,1 )] 


fit <- rpart(clase_ternaria ~ .,
		method="class", data=data)

printcp(fit) # display the results
plotcp(fit)

fit2 <- ctree(clase_ternaria ~ ., data=data)
print(fit2) # view results 

table(data2$clase_ternaria,predict(fit2, data2, type = c("response")))

table(data2$clase_ternaria,predict(fit, data2, type = c("class")))