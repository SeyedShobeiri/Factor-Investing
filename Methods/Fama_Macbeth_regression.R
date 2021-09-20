

Fama_Macbeth_regression = function(data,data_FF){
  
  # for now this regression is 5 factor model based on R1M_Usd
  # POSSIBLE UPGRADES : DO FM REGRESSION FOR DIFFERENT NUMBER OF FACTORS
  
  # data is the result of read_data.R function
  # data_FF is the result of read_FF5_data
  
  
  nb_factors = 5
  stock_ids = levels(as.factor(data$stock_id))
  stock_days = data %>% group_by(stock_id) %>% summarize(nb = n())
  stock_ids_short = stock_ids[which(stock_days$nb == max(stock_days$nb))]
  
  returns = data_ml %>% 
    filter(stock_id %in% stock_ids_short) %>% 
    dplyr::select(date,stock_id,R1M_Usd) %>% 
    spread(key = stock_id,value = R1M_Usd)  # can use pivot_wider instead of spread as well
  
  data_FM <- left_join(data %>% 
                         dplyr::select(date,stock_id,R1M_Usd) %>% 
                         filter(stock_id %in% stock_ids_short),
                       data_FF,
                       by = "date") %>% 
    mutate(R1M_Usd = lag(R1M_Usd)) %>% 
    na.omit() %>% 
    spread(key = stock_id,value = R1M_Usd)
  
  models <- lapply(paste0("`",stock_ids_short,'` ~ MKT_RF + SMB + HML + RMW + CMA'),
  function(f){lm(as.formula(f),data=data_FM,na.action="na.exclude") %>% 
      summary() %>% 
      "$"(coef) %>% 
      data.frame() %>% 
      dplyr::select(Estimate)}
  )
  
  betas <- matrix(unlist(models),ncol = nb_factors + 1,byrow = T) %>% 
    data.frame(row.names = stock_ids_short)
  
  colnames(betas) <- c("Constant","MKT_RF","SMB","HML","RMW","CMA")
  
  loadings <- betas %>% # Start from loadings (betas)
    dplyr::select(-Constant) %>% # Remove constant
    data.frame() # Convert to dataframe
  ret <- returns %>% # Start from returns
    dplyr::select(-date) %>% # Keep the returns only
    data.frame(row.names = returns$date) %>% # Set row names
    t() # Transpose
  FM_data <- cbind(loadings, ret)
  
  models <- lapply(paste("`",returns$date,"`",' ~ MKT_RF + SMB + HML + RMW + CMA',sep=""),
                   function(f){lm(as.formula(f),data=FM_data) %>% 
                       summary() %>% 
                       "$"(coef) %>% 
                       data.frame() %>% 
                       dplyr::select(Estimate)}
                   )
  gammas <- matrix(unlist(models),ncol = nb_factors + 1, byrow = T) %>% 
    data.frame(row.names = returns$date)
  colnames(gammas) <- c("Constant","MKT_RF","SMB","HML","RMW","CMA")
  
  
  out = list("DataFM" = data_FM,"Betas"=betas,"Gammas"=gammas)
  return(out)
}