library(tidyverse)
library(lubridate)
library(miceadds)

# Function to read an ml_data.RData table 
# inputs should contain quotation marks
read_data_ml <- function(path,begin,end,arrange_by){
  name = miceadds::load.Rdata2(path)
  name = name %>% 
    filter(date > begin,date < end) %>% 
    arrange(arrange_by,date)
  
  return(name)
}