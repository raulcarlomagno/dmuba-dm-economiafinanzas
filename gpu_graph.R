library(ggplot2)
library(tidyr)

gpu <- c('local(940mx)', 'k80', 'p100', 'p4')
mes1 <- c(123,66,25,33)
meses4 <- c(51,26,12,12)

df <- data.frame(gpu, mes1, meses4)
df$gpu <- as.factor(df$gpu)

df <-
	df %>%
	gather(key = tipo, value = segundos, -gpu)

ggplot(data=df, aes(x=gpu, y=segundos, fill=tipo)) +
		geom_bar(stat = "identity", position=position_dodge())