library(quantmod)
library(xtable)
library(tidyverse)
library(lubridate)

read_FF5_data = function(begin,end){
  # reads and clean Fama French 5 factors data
  
  KF_website <- "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/"
  KF_file <- "ftp/F-F_Research_Data_5_Factors_2x3_CSV.zip"
  temp = tempfile()
  link = paste0(KF_website,KF_file)
  download.file(link,temp,quiet=TRUE)
  
  FF_factors <- read_csv(unz(temp, "F-F_Research_Data_5_Factors_2x3.csv"),skip = 3)  
    
  FF_factors = FF_factors %>% rename(date = "...1",MKT_RF=`Mkt-RF`) %>% mutate_at(vars(-date), as.numeric) %>% 
    mutate(date = ymd(parse_date_time(date, "%Y%m"))) %>% 
    mutate(date = rollback(date + months(1)))
  
  FF_factors <- FF_factors %>% mutate(MKT_RF = MKT_RF / 100, 
                                      SMB = SMB / 100,
                                      HML = HML / 100,
                                      RMW = RMW / 100,
                                      CMA = CMA / 100,
                                      RF = RF/100) %>%
    filter(date >= begin, date <= end)
  
  return(FF_factors)
}