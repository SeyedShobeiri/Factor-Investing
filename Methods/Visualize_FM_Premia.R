
visualize_Fama_Macbeth_premias = function(data){
  #data is the result of the gammas from Fama_Macbeth_regression.R
  date = data$DataFM['date']
  data$Gammas %>% dplyr::select('MKT_RF','SMB','HML') %>% 
    bind_cols(date = date) %>% 
    gather(key = factor,value = gamma,-date) %>% 
    ggplot(aes(x=date,y=gamma,color=factor)) +
    geom_line() + facet_grid(factor~.)+
    scale_color_manual(values=c("#F87E1F", "#0570EA", "#F81F40")) + 
    coord_fixed(980)
}