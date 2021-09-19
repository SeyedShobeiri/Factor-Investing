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

data_ml %>% filter(date == "2000-02-29") %>% 
  ggplot(aes(x=Div_Yld)) + geom_histogram(bins = 100) + coord_fixed(0.03)

data_ml <- data_ml %>% group_by(date) %>% 
  mutate(R1M_Usd_C = R1M_Usd > median(R1M_Usd),
         R12M_Usd_C = R12M_Usd > median(R12M_Usd)) %>% ungroup() %>% mutate_if(is.logical,as.factor)

separation_date = as.Date("2014-01-15")
training_sample = filter(data_ml,date < separation_date)
testing_sample = filter(data_ml,date >= separation_date)

stock_ids = levels(as.factor(data_ml$stock_id))
stock_days = data_ml %>% group_by(stock_id) %>% summarize(nb = n())
stock_ids_short = stock_ids[which(stock_days$nb == max(stock_days$nb))]

returns = data_ml %>% 
  filter(stock_id %in% stock_ids_short) %>% 
  dplyr::select(date,stock_id,R1M_Usd) %>% 
  spread(key = stock_id,value = R1M_Usd)  # can use pivot_wider instead of spread as well


data_ml %>%
  group_by(date) %>%
  mutate(large = Mkt_Cap_12M_Usd > median(Mkt_Cap_12M_Usd)) %>% # Creates the cap sort
  ungroup() %>% # Ungroup
  mutate(year = lubridate::year(date)) %>% # Creates a year variable
  group_by(year, large) %>% # Analyze by year & cap
  summarize(avg_return = mean(R1M_Usd)) %>% # Compute average return
  ggplot(aes(x = year, y = avg_return, fill = large)) + # Plot!
  geom_col(position = "dodge") + # Bars side-to-side
  theme(legend.position = c(0.8, 0.2)) + # Legend location
  coord_fixed(124) + theme(legend.title=element_blank()) + # x/y aspect ratio
  scale_fill_manual(values=c("#F87E1F", "#0570EA"), name = "", # Colors
                    labels=c("Small", "Large")) +
  ylab("Average returns") + theme(legend.text=element_text(size=9))





library(quantmod) # Package for data extraction
library(xtable) # Package for LaTeX exports
min_date <- "1963-07-31" # Start date
max_date <- "2020-03-28" # Stop date
temp = tempfile()
KF_website <- "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/"
KF_file <- "ftp/F-F_Research_Data_5_Factors_2x3_CSV.zip"
link <- paste0(KF_website,KF_file) # Link of the file
download.file(link,temp, quiet = TRUE) # Download!
FF_factors <- read_csv(unz(temp,"F-F_Research_Data_5_Factors_2x3.csv"),
                       skip = 3) %>% # Check the number of lines to skip!
  rename(date = X1, MKT_RF = `Mkt-RF`) %>% # Change the name of first columns
  mutate_at(vars(-date), as.numeric) %>% # Convert values to number
  mutate(date = ymd(parse_date_time(date, "%Y%m"))) %>% # Date in right format
  mutate(date = rollback(date + months(1))) # End of month date
FF_factors <- FF_factors %>% mutate(MKT_RF = MKT_RF / 100, # Scale returns
                                    SMB = SMB / 100,
                                    HML = HML / 100,
                                    RMW = RMW / 100,
                                    CMA = CMA / 100,
                                    RF = RF/100) %>%
  filter(date >= min_date, date <= max_date) # Finally, keep only recent points
knitr::kable(head(FF_factors), booktabs = TRUE,
             caption = "Sample of monthly factor returns.") # A look at the data (see table)































