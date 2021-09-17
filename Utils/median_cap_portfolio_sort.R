library(tidyverse)
library(lubridate)
library(dplyr)
# this function implement simplified portfolio sorts using median

median_cap_portfolio_sort = function(path,begin,end,arrange_by,variable,returns){
  variable = enquo(variable)
  returns = enquo(returns)
  data = read_data_ml(path,begin,end,arrange_by)
  data %>% group_by(date) %>% 
    mutate(large = !!variable > median(!!variable)) %>% 
    ungroup() %>% 
    mutate(year = lubridate::year(date)) %>% 
    group_by(year,large) %>% 
    summarize(avg_return = mean(!!returns)) %>% 
    ggplot(aes(x=year,y=avg_return,fill = large)) +
    geom_col(position="dodge") +
    theme(legend.position = c(0.8,0.2)) +
    coord_fixed(124) + theme(legend.title=element_blank()) +
    scale_fill_manual(values = c("#F87E1F", "#0570EA"), name = "",labels=c("Small", "Large")) +
    ylab("Average returns") + theme(legend.text=element_text(size=9))
  
}