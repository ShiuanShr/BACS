
cars <- read.table("auto-data .txt", header=FALSE, na.strings = "?")
names(cars) <- c("mpg", "cylinders", "displacement", "horsepower", "weight", 
                 "acceleration", "model_year", "origin", "car_name")


cars$car_name <- NULL
#cars$origin <-as.factor(cars$origin)
cars<-na.omit(cars)
head(cars,10)

View(cars)



## Easy Demo
# Fitted Value

X <- cbind('(intercept)' = rep(1, nrow(cars)) , displacement = cars$displacement)

# why add intercept?
y <- cars$mpg



# Visualize the dataset first
plot(X[, 'displacement'], y, pch = 19, col = 'lightgray')
plot(X[, 'displacement'], y, pch = 19, col = 'cornflowerblue')
plot(X[, 'displacement'], y, type = 'l', pch = 19, col = 'cornflowerblue')


# 用Linear Algebra:來測量coefficient, 本質上belta會等同lm()的預測值

beta_hat <- solve(t(X) %*% X) %*% t(X) %*%y ; beta_hat

# 取得y prediction方法1
mpg_fitted <- fitted(mpg_lm) 

# 取得y prediction方法2
mpg_lm$fitted.values

head(data.frame(mpg_fitted, mpg_lm$fitted.values )) # Show it (比較好理解)



# 取得y prediction方法3
beta_hat <- solve(t(X) %*% X) %*% t(X) %*%y ; beta_hat
y_hat <- X %*% beta_hat 


# head(data.frame(y,y_hat)) # Show it (比較好理解)


# Visualize the dataset first
plot( cars$displacement, cars$mpg, pch = 19, col = 'lightgray')
points( cars$displacement, mpg_fitted , pch = 19, col = 'cornflowerblue')
# We'll get a stright lline cuz we are doing a "linear" regression , and the mpg_fitted is the model predicted.
# Basically, it's a linear regression line


# Residual = fit_error = SSE  != SST (注意!!)
fit_error <- y - y_hat
fit_error <- cars$mpg - mpg_fitted
head(data.frame(y, y_hat, fit_error))

mean(fit_error) # 應該要是0
mpg_lm <- lm(mpg ~ displacement, data=cars)
mpg_lm # should be the same as belta_hat


# 重要的是觀察slope, eg. displacement  slope =   -0.06005  


mpg_fitted <- fitted(mpg_lm)
mpg_fitted

#-------------------------------

## Evaluating Model Fit 

# 觀點1. 計算R square  , 觀點2. 計算MSE: 當有兩個模型相比時，MSE
較小的勝
#(R square = 1 - SSE/SST
# ps . SSE 可以被看作 error的離散程度 , error's variance
summary(mpg_lm) #  Adjusted R-squared:  0.6473 

# 求解 MSE = mean of  SSE = mean(sum((y - y_hat)^2)))
# Method 1

mean((cars$mpg - fitted(mpg_lm))^2)

# Method 2
mean(residuals(mpg_lm)^2)



# Import data

# Load the data and remove missing values
cars <- read.table("auto-data .txt", header=FALSE, na.strings = "?")
names(cars) <- c("mpg", "cylinders", "displacement", "horsepower", "weight", "acceleration",
                 "model_year", "origin", "car_name")
cars$car_name <- NULL # Drop y 
cars <- na.omit(cars) # Drop NA

# Shuffle the rows of cars
set.seed(27935752)
cars <- cars[sample(1:nrow(cars)),]


# Create a log transformed dataset also
cars_log <- with(cars, data.frame(log(mpg), log(cylinders), log(displacement), log(horsepower), log(weight), log(acceleration), model_year, origin))
# Linear model of mpg over all the variables that don’t have multicollinearity
cars_lm <- lm(mpg ~ weight + acceleration + model_year + factor(origin), data=cars)

# Linear model of log mpg over all the log variables that don’t have multicollinearity
cars_log_lm <- lm(log.mpg. ~ log.weight. + log.acceleration. + model_year + factor(origin), data=cars_log)

# Linear model of log mpg over all the log variables, including multicollinear terms!
cars_log_full_lm <- lm(log.mpg. ~ log.cylinders. + log.displacement. + log.horsepower. +
                           log.weight. + log.acceleration. + model_year + factor(origin),
                       data=cars_log)

## K folds Validation


# The input: structual dataframe and the specific k value
# it return the MSEOOS
k <- 10

fold_i_pe <- function(i, k, dataset, model) {
    folds <- cut(seq(1,nrow(dataset)),breaks=10,labels=FALSE)
    print(i)
    testIndexes <- which(folds==i,arr.ind=TRUE)
    test_set <- dataset[testIndexes, ]
    train_set <- dataset[-testIndexes, ]
    trained_model <- lm(model, data=train_set)
    predictions <- predict(trained_model, test_set)
    test_set$mpg - predictions
}

fold_pred_errors <- sapply(1:k, \(i) {
        fold_i_pe(i, k, cars, cars_lm)
    })
pred_errors <- unlist(fold_pred_errors)
mean(pred_errors^2)


#####
# Calculates mse_oos across all folds
k_fold_mse<-function(dataset, k=10, model) {
    fold_pred_errors<-sapply(1:k, \(i){
        fold_i_pe(i, k, dataset, model)
    })
    pred_errors<-unlist(fold_pred_errors)
    mean(pred_errors^2)
}
# Calculates prediction error for fold i out of k
fold_i_pe <- function(i, k, dataset, model) {
    folds <- cut(1:nrow(dataset), k, labels = FALSE)
    test_indices <- which(folds == i)
    test_set <- dataset[test_indices,]
    train_set <- dataset[-test_indices, ]
    trained_model <- lm(model, data = train_set)
    predictions <- predict(trained_model, test_set)
    actuals <- test_set
    pred_errors <- actuals - predictions
}

cars_lm_mse <- k_fold_mse(cars, k=10, cars_lm)
cars_lm_mse

