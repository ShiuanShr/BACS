# temp

df <- read.csv('insurance.csv', header = T)
insurance <- na.omit(df)
str(insurance)
insurance_model <- rpart(charges ~ age+sex+bmi +children +smoker +region, data = insurance )
head(insurance)



set.seed(22) # 此處必須設定seed
train.index <- sample(x=1:nrow(insurance), size=ceiling(0.8*nrow(insurance) ))
Train <- insurance[train.index, ]
head(Train)
Test <- insurance[-train.index, ]


#######################################################################
bagged_learn <- function(model, dataset, b=100, OLSmodel = TRUE ) {
    
    lapply(1:b, \(i) {
        Index <- sample(x=1:nrow(dataset), size=ceiling(0.8*nrow(dataset)), replace = T)
        Train=dataset[Index,]
        
        if (OLSmodel == T){
            Model <- update(model, data= Train)
        }
        else{
            Model<-rpart(charges~.,data=Train ,method='anova')
        }
    })
}

bagged_predict <- function(bagged_models, Testdata) {
    predictions <- lapply(bagged_models, \(x) predict(x, Testdata))
    df <- as.data.frame(predictions) |> apply(1, mean) |>as.data.frame()
    rownames(df) <- NULL
    colnames(df) <- NULL
    df
}

model_list <- bagged_learn(OLS_model,data = insurance, b =100, OLSmodel = TRUE)
bagged_prediction <- bagged_predict(model_list, Test)
# head(bagged_prediction)

actual <- test$charges
RMSoos_OLS <- (mean((unlist(bagged_prediction)- actual)^2))^.5
RMSoos_OLS
