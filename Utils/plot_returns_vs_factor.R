#Note : not working at this moment

plot_return_vs_factor = function(data,returns,factor){
  #data : data_ml
  
  data %>% 
    ggplot(aes(x=!!factor,y=!!returns))
    
}