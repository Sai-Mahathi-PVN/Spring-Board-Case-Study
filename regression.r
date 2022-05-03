install.packages("plyr")
install.packages("readr")
install.packages("dplyr")
install.packages("caret")
install.packages("readxl")
install.packages("data.table")
install.packages("mltools")
install.packages("writexl")
install.packages("caTools")
install.packages("rpart.plot")
install.packages("car")
library(readxl)
library(car)
library(plyr)
library(readr)
library(dplyr)
library(caret)
library(data.table)
library(mltools)
library(writexl)
library(caTools)
library(rpart)
library(rpart.plot)

sdf <- read_excel("sales_data.xlsx",sheet = "sales_data")
glimpse(sdf)
boxplot(sdf$sales_dollars_value, sdf$sales_units_value, 
        sdf$sales_lbs_value, sdf$`Google Search Data`, sdf$`Social Media Data`)


####MODEL CONSTRUCTION

#Converting categorical variables to factor
j <- c("MONTH","WEEK NO.", "QUARTER","Claim ID", "Claim Name","Vendor")
sdf[j] <- lapply(sdf[j], factor)

#Scaling Numeric variables
sdf$sales_dollars_value <- c(scale(sdf$sales_dollars_value))
sdf$sales_units_value <- c(scale(sdf$sales_units_value))
sdf$sales_lbs_value <- c(scale(sdf$sales_lbs_value))
sdf$`Google Search Data` <- c(scale(sdf$`Google Search Data`))
sdf$`Social Media Data` <- c(scale(sdf$`Social Media Data`))

#Selecting a set of significant variables for constructing model
df1 <- subset (sdf, select = -c(system_calendar_key_N,product_id, DATE,
                                `Claim Name`,`WEEK NO.`, MONTH))

smp_size <- floor(0.80 * nrow(df1))
set.seed(123)
train_ind <- sample(seq_len(nrow(df1)), size = smp_size)

training_set <- df1[train_ind, ]
testing_set <- df1[-train_ind, ]

glimpse(training_set)
glimpse(testing_set)

##Evaluation Metrics Calculation
eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_squared <- 1 - SSE / SST
  Adjusted_R_squared <- 1 - (1 - R_squared) * ((nrow(df) - 1)/(nrow(df) - 7 - 1))
  RMSE = sqrt(SSE/nrow(df))
  # Model performance metrics
  data.frame(
    RMSE <- RMSE,
    Rsquared <- R_squared,
    Adjusted_R_Squared <- Adjusted_R_squared
  )
}

regressor = lm(formula = sales_dollars_value ~ . , 
               data = training_set)
summary(regressor)

##REGRESSION TREE

regression_tree <- rpart(sales_dollars_value ~ . , data = training_set, 
              control=rpart.control(cp=.0001))

predictions_test_cart <- predict(regression_tree, newdata = testing_set)
eval_results(testing_set$sales_dollars_value, predictions_test_cart, testing_set)


##RIDGE REGRESSION
install.packages("glmnet")
install.packages("Matrix")
library(glmnet)

x = data.matrix(subset (training_set, select = -c(sales_dollars_value)))
y_train = training_set$sales_dollars_value

x_test = data.matrix(subset (testing_set, select = -c(sales_dollars_value)))
y_test = testing_set$sales_dollars_value

lambdas <- 10^seq(2, -3, by = -.1)
cv_ridge_reg = cv.glmnet(x, y_train, nlambda = 25, alpha = 0,lambda = lambdas, 
                         family="gaussian")
optimal_lambda <- cv_ridge_reg$lambda.min
optimal_lambda


# Prediction and evaluation on train data
predictions_train <- predict(cv_ridge_reg, s = optimal_lambda, newx = x)
eval_results(y_train, predictions_train, training_set)
# Prediction and evaluation on test data
predictions_test <- predict(ridge_reg, s = optimal_lambda, newx = x_test)
eval_results(y_test, predictions_test, testing_set)


##LASSO REGRESSION
lambdas <- 10^seq(2, -3, by = -.1)

# Setting alpha = 1 implements lasso regression
lasso_reg <- cv.glmnet(x, y_train, alpha = 1, lambda = lambdas, standardize = TRUE,
                       nfolds = 5)

# Best 
lambda_best <- lasso_reg$lambda.min 
lambda_best

lasso_model <- glmnet(x, y_train, alpha = 1, lambda = lambda_best, standardize = TRUE)

predictions_train <- predict(lasso_model, s = lambda_best, newx = x)
eval_results(y_train, predictions_train, training_set)

predictions_test <- predict(lasso_model, s = lambda_best, newx = x_test)
eval_results(y_test, predictions_test, testing_set)

##ELASTIC NET REGRESSION

train_cont <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 5,
                           search = "random",
                           verboseIter = TRUE)



elastic_reg <- train(sales_dollars_value ~ .,
                     data = training_set,
                     method = "glmnet",
                     preProcess = c("center", "scale"),
                     tuneLength = 10,
                     trControl = train_cont)


elastic_reg$bestTune

# Make predictions on training set
predictions_train <- predict(elastic_reg, x)
eval_results(y_train, predictions_train, training_set) 

# Make predictions on test set
predictions_test <- predict(elastic_reg, x_test)
eval_results(y_test, predictions_test, testing_set)