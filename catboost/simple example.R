library(catboost)

train_path = system.file("extdata", 
		"adult_train.1000", 
		package="catboost")
test_path = system.file("extdata", 
		"adult_test.1000", 
		package="catboost")

column_description_vector = rep('numeric', 15)
cat_features <- c(3, 5, 7, 8, 9, 10, 11, 15)
for (i in cat_features)
	column_description_vector[i] <- 'factor'

train <- read.table(train_path, 
		head = F, 
		sep = "\t", 
		colClasses = column_description_vector, 
		na.strings='NAN')

test <- read.table(test_path, 
		head = F, 
		sep = "\t", 
		colClasses = column_description_vector, 
		na.strings='NAN')

target <- c(1)

train_pool <- catboost.load_pool(data=train[,-target], label = train[,target])

test_pool <- catboost.load_pool(data=test[,-target], label = test[,target])

train_pool
test_pool

fit_params <- list(iterations = 100,
		thread_count = 10,
		#loss_function = 'Logloss:Border=0.5',
		loss_function = 'Logloss',
		#custom_loss = 'AUC', #no anda
		task_type = 'GPU',
		ignored_features = c(4,9),
		border_count = 32,
		depth = 5,
		learning_rate = 0.02,
		l2_leaf_reg = 3.5,
		train_dir = 'train_dir')

#model <- catboost.train(train_pool, test_pool, fit_params)
model <- catboost.train(train_pool, NULL, fit_params)

#prediction <- catboost.predict(model, test_pool, prediction_type = 'RawFormulaVal')


catboost.get_feature_importance(model, 
		pool = NULL, 
		fstr_type = "FeatureImportance",
		thread_count = -1)

