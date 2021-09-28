library(glmnet)

ridge_or_lasso_reg = function(data,target,num_of_features,alpha){
  # RESULT: LASSO regression
  # we assume the target is the last column of the data table
  # num_of_feature is used to subset all the features
  # alpha = 1 ====> lasso           alpha = 0 =====> ridge
  
  y_penalized <- data[[target]]
  x_penalized <- data[1:num_of_features]
  fit_lasso <- glmnet(x_penalized,y_penalized,alpha=alpha)
  
  return(fit_lasso)
}