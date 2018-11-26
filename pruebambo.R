rm(list = ls())
invisible(gc())

library(catboost)
library(mlrMBO)

#setwd('D:\\maestriadm\\dm economia finanzas\\bankchurn')
setwd('/home/rcarlomagno/catboost')

source('config.R')
source('dataset_sql.R')
source('functions.R')

train_periods <- c(201802,201801,201712,201704,201703,201702)
#train_periods <- c(201704, 201703, 201702)

data_train <- get_period(train_periods)

test_periods <- c(201804)
data_test <- get_period(test_periods)

useless_columns <- c('numero_de_cliente', 'foto_mes')

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

catboost_train <- function(x = list(
  depth,
  iterations,
  border_count,
  l2_leaf_reg,
  learning_rate,
  #rsm,
  #cutoff,
  #cutoff_in_logloss,
  random_strength,
  bagging_temperature
)){
  
  loss_function <- 'Logloss'
  #if(x$cutoff_in_logloss == 1)
  #loss_function <- paste0(loss_function, ':border=', x$cutoff)
  
  #https://effectiveml.com/using-grid-search-to-optimise-catboost-parameters.html
  fit_params <- list(
    iterations = x$iterations,
    loss_function = loss_function,
    #loss_function = 'Logloss',
    #custom_loss = c('Logloss', 'AUC'),
    task_type = 'GPU',
    border_count = x$border_count,
    depth = x$depth,
    learning_rate = x$learning_rate,
    l2_leaf_reg = x$l2_leaf_reg,
    #rsm = x$rsm,
    train_dir = CONFIG$TRAIN_DIR,
    logging_level = 'Verbose',
    #logging_level = 'Silent',
    random_strength = x$random_strength,
    bagging_temperature = x$bagging_temperature
  )
  
  cat("training with these hyperparameters:\n")
  print(data.frame(x), row.names = FALSE )
  
  #model <- catboost.train(train_pool, test_pool, fit_params)
  model <- catboost.train(train_pool, NULL, fit_params)
  
  predictions_prob_testing <- catboost.predict(model, test_pool, prediction_type = 'Probability') 
  
  invisible(gc())
  
  #profit <- calculate_profit(x$cutoff, predictions_prob_testing, data_test$target)
  profit <- calculate_profit(CONFIG$DEFAULT_CUTOFF, predictions_prob_testing, data_test$target)
  perfect_profit <- sum(data_test$target) * 11700
  
  print(paste0("profit: $", formatC(profit, format="f", digits=0, big.mark=".", decimal.mark = ',')))
  
  profit_ratio_cutoff <- round(profit / perfect_profit, 6)
  
  print(paste('Perfect profit ratio cutoff', profit_ratio_cutoff))
  
  return(profit_ratio_cutoff)
}


configureMlr(show.learner.output = FALSE)

#configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
#https://github.com/catboost/benchmarks/blob/master/quality_benchmarks/comparison_description.pdf
objetive_function <- makeSingleObjectiveFunction(
  name = "catboost_optimizer",
  fn   = catboost_train,
  par.set = makeParamSet(
    makeIntegerParam("depth", lower = 6L, upper = 10L, default = 6L),
    makeIntegerParam("iterations", lower = 250L, upper = 1250L, default = 1000L),
    #makeIntegerParam("iterations", lower = 1L, upper = 10L),
    makeIntegerParam("border_count", lower = 128L, upper = 254L, default = 128L),
    #makeIntegerParam("border_count", lower = 2L, upper = 254L), #subir fix para avisar que con 255 no va. Error in catboost.train(train_pool, NULL, fit_params) : c:/goagent/pipelines/buildmaster/catboost.git/catboost/cuda/gpu_data/compressed_index_builder.h:110: Error: can't proceed some features ,  Error: border count should be greater than 0. If you have nan-features, border count should be > 1. Got 1"
    makeIntegerParam("l2_leaf_reg", lower = 1, upper = 10, default = 3),
    makeNumericParam("learning_rate", lower = 1e-07, upper = 1, default = 0.03), #o 1e-06
    #makeNumericParam("rsm", lower = 0.01, upper = 1), #rsm on GPU is supported for pairwise modes only
    #makeNumericParam("cutoff", lower = 0.01, upper = 0.1),
    #makeIntegerParam("cutoff_in_logloss", lower = 0L, upper = 1L), #la regresion Kriging no soporta makeLogicalParam
    makeIntegerParam("random_strength", lower = 1, upper = 20, default = 1),
    makeIntegerParam("bagging_temperature", lower = 0, upper = 1, default = 1)
  ),
  minimize = FALSE,
  has.simple.signature = FALSE,
  global.opt.value = -1
)


mbo_iterations = 200

mbo_control <-  makeMBOControl(propose.points = 1L)
mbo_control <-  setMBOControlTermination(mbo_control, iters = mbo_iterations)
mbo_control <-  setMBOControlInfill(mbo_control, crit = makeMBOInfillCritEI(), opt  = "focussearch", opt.focussearch.points = mbo_iterations)

#matern3_2
surrogate_learner <-  makeLearner("regr.km", predict.type = "se", covtype = "matern5_2", control = list(trace = FALSE))
mbo_learner  <- makeMBOLearner(mbo_control, objetive_function)

mbo_start_time <- Sys.time()
mbo_result  <-  mbo(objetive_function, learner = surrogate_learner, control = mbo_control, design = NULL)
mbo_end_time <- Sys.time()

mbo_end_time - mbo_start_time

cat("best hyperparameters:\n")
print(data.frame(mbo_result$x), row.names = FALSE )
#print(paste0("profit: $", formatC(mbo_result$y, format="f", digits=0, big.mark=".", decimal.mark = ',')))
print(paste0("Perfect profit ratio cutoff: ", mbo_result$y))

png(paste0(CONFIG$TRAIN_DIR, "mbo.png"), width = 1920, height = 1080)
plot(mbo_result)
dev.off()


