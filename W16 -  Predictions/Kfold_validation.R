
k_fold_mse <- function(dataset, k=10, model) {
    
    # shuffle the data
    set.seed(27935752)
    data_shuffle <- dataset[sample(1:nrow(dataset),replace = F),]
    fold_pred_errors <- sapply(1:k, \(i) {
        fold_i_pe(i, k, data_shuffle, model)
    })
    pred_errors <- unlist(fold_pred_errors)
    mean(pred_errors^2)
}

fold_i_pe <- function(i, k, dataset, model) {
    folds <- cut(seq(1,nrow(dataset)),breaks=k,labels=FALSE)
    testIndexes <- which(folds==i,arr.ind=TRUE)  # find the index of the list that match the condition
    
    test_set <- dataset[testIndexes, ]
    train_set <- dataset[-testIndexes, ]
    
    f <- format(terms(model)) %>% paste(., collapse = " ") %>% as.formula()
    trained_model <- lm(f,train_set)
    formula <- trained_model$model %>% names() # ¨ú¥XYªº column name
    predictions <- predict(trained_model, test_set)
    real_value <- test_set[,colnames(test_set) == formula[1]]
    real_value - predictions
}