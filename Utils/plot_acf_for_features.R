
plot_acf_for_features = function(data){
  # data : data_ml
  features <- colnames(data_ml[3:95])
  autocorrs <- data_ml %>% 
    dplyr::select(c("stock_id", features)) %>% 
    gather(key = feature, value = value, -stock_id) %>% 
    group_by(stock_id, feature) %>% 
    summarize(acf = acf(value, lag.max = 1, plot = FALSE)$acf[2]) 
  autocorrs %>% ggplot(aes(x = acf)) + xlim(-0.1,1) + 
    geom_histogram(bins = 60)
}