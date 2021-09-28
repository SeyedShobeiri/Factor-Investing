library(tidyverse)
library(lubridate)
load("Datasets/data_ml.RData")


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




nb_factors <- 5 # Number of factors
data_FM <- left_join(data_ml %>% # Join the 2 datasets
                       dplyr::select(date, stock_id, R1M_Usd) %>% # (with returns...
                       filter(stock_id %in% stock_ids_short), # ... over some stocks)
                     FF_factors,
                     by = "date") %>%
  mutate(R1M_Usd = lag(R1M_Usd)) %>% # Lag returns
  na.omit() %>% # Remove missing points
  spread(key = stock_id, value = R1M_Usd)
models <- lapply(paste0("`", stock_ids_short,
                        '` ~ MKT_RF + SMB + HML + RMW + CMA'),
                 function(f){ lm(as.formula(f), data = data_FM, # Call lm(.)
                                 na.action="na.exclude") %>%
                     summary() %>% # Gather the output
                     "$"(coef) %>% # Keep only coefs
                     data.frame() %>% # Convert to dataframe
                     dplyr::select(Estimate)} # Keep the estimates
)
betas <- matrix(unlist(models), ncol = nb_factors + 1, byrow = T) %>% # Extract the betas
  data.frame(row.names = stock_ids_short) # Format: row names
colnames(betas) <- c("Constant", "MKT_RF", "SMB", "HML", "RMW", "CMA")

loadings <- betas %>% # Start from loadings (betas)
  dplyr::select(-Constant) %>% # Remove constant
  data.frame() # Convert to dataframe
ret <- returns %>% # Start from returns
  dplyr::select(-date) %>% # Keep the returns only
  data.frame(row.names = returns$date) %>% # Set row names
  t() # Transpose
FM_data <- cbind(loadings, ret)




data_ml %>%
  group_by(date) %>%
  mutate(growth = Pb > median(Pb)) %>% # Creates the cap sort
  ungroup() %>% # Ungroup
  mutate(year = lubridate::year(date)) %>% # Creates a year variable
  group_by(year, growth) %>% # Analyze by year & cap
  summarize(avg_return = mean(R12M_Usd)) %>% # Compute average return
  ggplot(aes(x = year, y = avg_return, fill = growth)) + # Plot!
  geom_col(position = "dodge") + # Bars side-to-side
  theme(legend.position = c(0.8, 0.2)) + # Legend location
  theme(legend.title=element_blank()) + # x/y aspect ratio
  scale_fill_manual(values=c("#F87E1F", "#0570EA"), name = "", # Colors
                    labels=c("Small", "Large")) +
  ylab("Average returns") + theme(legend.text=element_text(size=9))


returns_m <- data_ml %>%
    group_by(date) %>%
    mutate(growth = Pb > median(Pb)) %>% # Creates the cap sort
    mutate(year = lubridate::year(date)) %>% # Creates a year variable
    group_by(year, growth) %>% # Analyze by year & cap
    summarize(avg_return = mean(R12M_Usd)) %>% # Compute average return
    spread(key=growth,value=avg_return) %>% 
    ungroup()

colnames(returns_m)[2:3] <- c("value","growth")
returns_m %>% 
  mutate(value = cumprod(1+value),growth = cumprod(1+growth)) %>% 
  gather(key = portfolio, value = value,-year) %>% 
  ggplot(aes(x=year,y=value,color=portfolio)) + geom_line() + theme(legend.position = c(0.7,0.8))


data_ml %>% 
  mutate(small = Mkt_Cap_6M_Usd <= 0.25,
         medium = Mkt_Cap_6M_Usd > 0.25 & Mkt_Cap_6M_Usd <= 0.5,
         large = Mkt_Cap_6M_Usd > 0.5 & Mkt_Cap_6M_Usd <= 0.75,
         xl = Mkt_Cap_6M_Usd > 0.75,
         year = lubridate::year(date)) %>%
  group_by(year) %>%
  summarize(small = mean(small * R1M_Usd), # Compute avg returns
            medium = mean(medium * R1M_Usd),
            large = mean(large * R1M_Usd),
            xl = mean(xl * R1M_Usd)) %>%
  gather(key = size,value = return, -year) %>% 
  ggplot(aes(x = year, y = return, fill = size)) + geom_col(position = "dodge")
  


data_ml %>%
  dplyr::select(c(features_short, "R1M_Usd", "date")) %>% # Keep few features, label & date
  group_by(date) %>% # Group: dates!
  summarise_all(funs(cor(.,R1M_Usd))) %>% # Compute correlations
  dplyr::select(-R1M_Usd) %>% # Remove label
  gather(key = Predictor, value = value, -date) %>% # Put in tidy format
  ggplot(aes(x = Predictor, y = value, color = Predictor)) + # Plot
  geom_boxplot(outlier.colour = "black") + coord_flip() +
  theme(aspect.ratio = 0.6) + xlab(element_blank())



data_ml %>% 
  ggplot(aes(y=R1M_Usd)) +
  geom_smooth(aes(x=Mkt_Cap_12M_Usd,color = "Market Cap")) +
  geom_smooth(aes(x=Vol1Y_Usd,color="Volatility")) + 
  scale_color_manual(values = c("#F87E1F", "#0570EA")) +
  coord_fixed(10) +
  labs(color = "Predictor") + xlab(element_blank())


data_ml %>% # From dataset:
  dplyr::select(c("stock_id", features)) %>% # Keep ids & features
  gather(key = feature, value = value, -stock_id) %>% # Put in tidy format
  group_by(stock_id, feature) %>% # Group
  summarize(acf = acf(value, lag.max = 1, plot = FALSE)$acf[2])



Length <- 100
x <- exp(sin(1:Length))
data <- data.frame(index=1:Length,x = x)
ggplot(data,aes(x = index, y = x)) + geom_bar(stat = "identity")


norm_unif <- function(v){
  v <- v %>% as.matrix()
  return(ecdf(v)(v))
}

norm_0_1 <- function(v){
  return((v-min(v))/(max(v)-min(v)))
}

data_norm <- data.frame(
  index = 1:Length,
  original = x,
  standard = (x-mean(x))/sd(x),
  norm_0_1 = norm_0_1(x),
  unif = norm_unif(x)) %>% 
  gather(key = Type, value = value, -index)

ggplot(data_norm,aes(x=index,y=value,fill=Type)) + geom_bar(stat = "identity") + facet_grid(Type~.)
ggplot(data_norm,aes(x=value,fill=Type)) + geom_histogram(position="dodge") + facet_grid(Type~.)



firm <- c(rep(1,3), rep(2,3), rep(3,3)) # Firms (3 lines for each)
date <- rep(c(1,2,3),3) # Dates
cap <- c(10, 50, 100, # Market capitalization
         15, 10, 15,
         200, 120, 80)
return <- c(0.06, 0.01, -0.06, # Return values
            -0.03, 0.00, 0.02,
            -0.04, -0.02,0.00)
data_toy <- data.frame(firm, date, cap, return) # Aggregation of data
data_toy <- data_toy %>% # Transformation of data
  group_by(date) %>%
  mutate(cap_0_1 = norm_0_1(cap), cap_u = norm_unif(cap))


lm(return ~ cap_0_1,data=data_toy) %>% broom::tidy() %>% knitr::kable(caption = 'Regression output when the independent var. comes from min-max rescaling', booktabs = T)
lm(return ~ cap_u,data=data_toy) %>% broom::tidy() %>% knitr::kable(caption = 'Regression output when the independent var. comes from min-max rescaling', booktabs = T)


getSymbols.FRED("BAMLC0A0CM",env = ".GlobalEnv",return.class = "xts")
cred_spread <- fortify(BAMLC0A0CM)
colnames(cred_spread) <- c("date","spread")
cred_spread <- cred_spread %>% 
  full_join(data_ml %>% dplyr::select(date),by="date") %>% mutate(spread = na.locf(spread))
cred_spread <- cred_spread[!duplicated(cred_spread),]

data_cond <- data_ml %>% dplyr::select(c("stock_id","date",features_short))
names_cred_spread <- paste0(features_short,"_cred_spread")
feat_cred_spread <- data_cond %>% dplyr::select(features_short)
cred_spread <- data_ml %>% 
  dplyr::select(date) %>%
  left_join(cred_spread,by="date")
feat_cred_spread <- feat_cred_spread * 
  matrix(cred_spread$spread,length(cred_spread$spread),
         length(features_short))
colnames(feat_cred_spread) <- names_cred_spread
data_cond <- bind_cols(data_cond,feat_cred_spread)
data_cond %>% ggplot(aes(x=Eps_cred_spread)) + geom_histogram()
  

data_cond <- data_cond %>% 
  group_by(date) %>% 
  mutate_at(names_cred_spread,norm_unif)
data_cond %>% ggplot(aes(x=Eps_cred_spread)) + geom_histogram(bins=100)


getSymbols.FRED("VIXCLS",
                env = ".GlobalEnv",
                return.class = "xts")

vix = fortify(VIXCLS)
colnames(vix) <- c("date","vix")
vix <- vix %>% 
  full_join(data_ml %>% dplyr::select(date),by="date") %>% 
  mutate(vix=na.locf(vix))

vix <- vix[!duplicated(vix),]
vix <- data_ml %>% 
  dplyr::select(date) %>% 
  left_join(vix,by="date")



delta <- 0.5
vix_bar <- median(vix$vix)
data_vix <- data_ml %>% 
  dplyr::select(stock_id,date,R1M_Usd) %>% 
  mutate(r_minus = (-0.02) * exp(-delta*(vix$vix - vix_bar)),
         r_plus = 0.02 * exp(delta*(vix$vix-vix_bar)))

data_vix <- data_vix %>% 
  mutate(R1M_Usd_Cvix = if_else(R1M_Usd < r_minus, -1,
                                if_else(R1M_Usd > r_plus,1,0)),
         R1M_Usd_Cvix = as.factor(R1M_Usd_Cvix))

data_vix %>% 
  mutate(year = year(date)) %>% 
  group_by(year,R1M_Usd_Cvix) %>% 
  summarize(nb = n()) %>% 
  ggplot(aes(x=year,y=nb,fill=R1M_Usd_Cvix)) + geom_col()


data_ml %>% ggplot(aes(x=R12M_Usd)) + geom_histogram()
data_ml %>% filter(stock_id == 683,year(date) == 2009) %>% dplyr::select(date,Vol1Y_Usd)

library(glmnet)
y_penalized = data_ml$R1M_Usd
x_penalized = data_ml %>% 
  dplyr::select(all_of(features)) %>% as.matrix()
fit_lasso <- glmnet(x_penalized,y_penalized,alpha=1)


lasso_coef <- summary(fit_lasso$beta)
lambda <- fit_lasso$lambda
lasso_coef$Lambda <- lambda[lasso_coef$j]
lasso_coef$Feature <- features[lasso_coef$i] %>% as.factor()
lasso_coef[1:120,] %>% 
  ggplot(aes(x=Lambda,y=x,color=Feature)) + 
  geom_line() + coord_fixed(0.25) + ylab("beta") +
  theme(legend.text = element_text(size = 7))































