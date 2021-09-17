library(tidyverse)
library(lubridate)
load("data_ml.RData")


data_ml <- data_ml %>% filter(date > "1999-12-30",
                              date < "2019-01-01") %>% arrange(stock_id,date)
data_ml %>% 
  group_by(date) %>% 
  summarize(nb_assets = stock_id %>% 
              as.factor() %>% nlevels()) %>% 
  ggplot(aes(x=date,y=nb_assets)) + geom_col() + coord_fixed(3)
                  
features <- colnames(data_ml[3:95])
features_short <- c("Div_Yld", "Eps", "Mkt_Cap_12M_Usd", "Mom_11M_Usd",
                    "Ocf", "Pb", "Vol1Y_Usd")

data_ml %>% filter(date == "2000-01-01") %>% 
  ggplot(aes(x=Div_Yld)) + geom_histogram(bins = 100) + coord_fixed(0.03)