

plot_FF_returns = function(data){
  # data is the result of read_FF5_data function
  data %>% 
    mutate(date=year(date)) %>% 
    gather(key=factor,value=value,-date) %>% 
    group_by(date,factor) %>% 
    summarise(value = mean(value)) %>% 
    ggplot(aes(x=date,y=value,color=factor)) +geom_line() + coord_fixed(500)
}