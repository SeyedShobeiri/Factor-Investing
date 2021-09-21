
Find_Redundancies = function(data){
  #data : result of the read_FF5_data
  factors <- c("MKT_RF", "SMB", "HML", "RMW", "CMA")
  models <- lapply(paste(factors,' ~ MKT_RF + SMB + HML + RMW + CMA-',factors),
                   function(f){lm(as.formula(f),data = data) %>% 
                       summary() %>% 
                       "$"(coef) %>% 
                       data.frame() %>% 
                       filter(rownames(.) == "(Intercept)") %>% 
                       dplyr::select(Estimate,`Pr...t..`)}
                   )
  alphas <- matrix(unlist(models),ncol=2,byrow=T) %>% 
    data.frame(row.names = factors)
  
  results <- matrix(NA,nrow=length(factors),ncol=length(factors)+1)
  signif <- matrix(NA,nrow=length(factors),ncol=length(factors)+1)
  
  for (j in 1:length(factors)){
    form <- paste(factors[j],' ~ MKT_RF + SMB + HML + RMW + CMA-',factors[j])
    fit <- lm(form,data = data) %>% summary()
    coef <- fit$coefficients[,1]
    p_val = fit$coefficients[,4]
    results[j,-(j+1)] <- coef
    signif[j,-(j+1)] <- p_val 
  }
  
  signif[is.na(signif)] <- 1
  results <- results %>% round(3) %>% data.frame()
  results[signif < 0.001] <- paste(results[signif<0.001]," (***)")
  results[signif>0.001&signif<0.01] <- paste(results[signif>0.001&signif<0.01]," (**)")
  results[signif>0.01&signif<0.05] <- paste(results[signif>0.01&signif<0.05]," (*)")
  results <- cbind(as.character(factors), results) 
  colnames(results) <- c("Dep. Variable","Intercept", factors)
  
  return(results)
}