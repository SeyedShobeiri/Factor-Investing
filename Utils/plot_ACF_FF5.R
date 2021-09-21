
plot_ACF_FF5 = function(data,lag){
  #data is the result of read_FF5_data.R
  
  library(cowplot)
  library(forecast)
  
  acf_SMB <- ggAcf(data$SMB,lag.max=lag) + labs(title="")
  acf_HML <- ggAcf(data$HML,lag.max=lag) + labs(title="")
  acf_RMW <- ggAcf(data$RMW,lag.max=lag) + labs(title="")
  acf_CMA <- ggAcf(data$CMA,lag.max=lag) + labs(title="")
  
  plot_grid(acf_SMB,acf_HML,acf_RMW,acf_CMA,labels=c('SMB','HML','RMW','CMA'))
}