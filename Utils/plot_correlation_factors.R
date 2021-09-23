#Note : not working at this moment



plot_correlation_factors = function(data,returns,short_list_of_features){
  # data is the data_ml 
  data %>% 
    dplyr::select(c(!!short_list_of_features,!!returns,"date")) %>% 
    group_by(date) %>% 
    summarise_all(funs(cor(!!.,returns))) %>% 
    dplyr::select(-!!returns) %>%
    gather(key = Predictor,value = value, -date) %>%
    ggplot(aes(x=Predictor,y = value,color = Predictor)) +
    geom_boxplot(outlier.color = "black") + coord_filp() +
    theme(aspect.ratio = 0.6) + xlab(element_blank())
}