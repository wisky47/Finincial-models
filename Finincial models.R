
#
# useful models and examples
#
#
#

rm(list = ls())

#
# NAME : Wish 
#

######## some usefull libraries that will be required.

library(quantmod)
library(PerformanceAnalytics)
library(tseries)
library(ggplot2)
library(dplyr)
library(Quandl)
library(fable)
library(feasts)
library(tsibble)
library(urca)
library(broom)
library(gridExtra)

####### Question 1, get weekly data.

Quandl.api_key("*******************************") # enter api key to download data directly into xts type object.

xtsdata <- Quandl('LBMA/GOLD',collapse = "weekly", type = "xts")

data <- xtsdata # for backup purpose.

head(data)

# data operations to remove unnecessary columns. # when using na.omit() we don't loose much data as unnecessary column eliminated first..
# 

data<- data[,-c(1,3,4,5,6)]

data <- na.omit(data) # omitted NA values. from final data

head(data)

day <- as.Date(row.names(data.frame(data)))

########Question 2 creaating data frame data2 with day price and log return.

price <- data$`USD (PM)`

log_return <- c(100 * diff (log(price),lag = 5)) # calculating log returns. lag = 5 as working day in week = 5

#
# Multiplied with 100 so that we can see data in % form.
#

data_df <- data.frame(day,price,log_return)
x <- c("day","price","log_return")

colnames(data_df) <- x

data_df <- na.omit(data_df) # omiting NA values.

rownames(data_df) <- 1:nrow(data_df) # removing row names as dates. as we already have it in day column.

head(data_df) # data frame created.

##length(data_df$log_return)

########Question 3 plot 2 ts variables, test normality and compute histogram.
#
# Graph 1
price_p <- ggplot(data_df,aes(x=day,y=price)) + 
  geom_line(color="steelblue") + 
  xlab("") +
  ylab("")+
  scale_x_date(date_breaks = "4 year",date_labels = "%Y")
price_p

# Graph 2.
p_log_return <- ggplot(data_df,aes(x=day,y=log_return)) + 
  geom_line(color="green") + 
  xlab("") +
  ylab("")+
  theme_bw()+
  scale_x_date(date_breaks = "4 year",date_labels = "%Y")
p_log_return

# normality test 

skewness(data_df$log_return) # + ve positive.
kurtosis(data_df$log_return) # excess kurtosis (has fat tails.) kurtosis > 3
jarque.bera.test(data_df$log_return) # approx 0 p-val

#histogram.

Histo <- hist(data_df$log_return,breaks = 100,xlim = c(-10,10),col= "red")

########Question 4 days on which price and log return is min and max.

data_df$day[[which.max(data_df$price)]] # max and min price on day
data_df$day[[which.min(data_df$price)]] #
data_df$day[[which.max(data_df$log_return)]] # max and min log return on day
data_df$day[[which.min(data_df$log_return)]] #

# inconveniences like floating holidays are a problem to caclulation. 
# all  years are not of 52 weeks sometimes it can have 53 weeks.
# hard changes visible step-wise and not smooth as daily changes.

########Question 5 convert to monthly, why caution for interpretation. modern TS, yearmonth. remove NA vals.
#
#
# caution to interpretation because changes at week start and end of month will not be visible.
# instead overall changes on one month will be visible.
#

data_df <- mutate(data_df, day = yearmonth(day)) # year month date conversion
data_df <- data_df %>% group_by(day) %>% summarise_all(mean) # aggregating all vals.
data_df <- as_tsibble(data_df, index = day) # modern time-series data set converted.
data_df <- na.omit(data_df) # remove NA vals.

head(data_df)

length(data_df$price)

########Question 6 graphically check auto correlation. what can be inferred.

afc_lg_rtn <- data_df$log_return %>%
  acf(lag = 360) # ACF
plot(afc_lg_rtn[1:360], main= "") # ZOOM view

acf_abs_lg_rtn <- acf(abs(data_df$log_return), lag= 360) # ACF absolute returns
plot(acf_abs_lg_rtn[1:360], main= "")  

pacf_lg_rtn <- data_df$log_return %>%
  pacf(lag = 360) #PACF
plot(pacf_lg_rtn[1:360], main="")

#
# seems to be an AR MA process.
# ACF breaks (zoom in to see gap is small) => MA
# and ACF also converges towards 0 => AR
#

########Question 7 Check the variable for the existence of a trend. Explain

#
# Unit root test in log-levels with to find if trend exists or not.

summary(ur.df(data_df$log_return, selectlags = "AIC", type ="trend"))
summary(ur.df(data_df$log_return, selectlags = "BIC", type ="trend"))

#' trend is non-significant at 5% . t-stat is in the rejection area
#' data stationary . unit root not present . 
#' reject Ho

########Question 8  ARIMA(0,0,0) - ARIMA(4,0,4) models. which minizs iC. why d=0?

lg_return <- log_return # to avoid variable confusion below.
rm(log_return)

arma <- data_df %>% # possible combinations of AR(p) and MA(q) orders
  model(
    arma00 = ARIMA(log_return ~ 1 + pdq(0, 0, 0) + PDQ(0, 0, 0)),
    arma10 = ARIMA(log_return ~ 1 + pdq(1, 0, 0) + PDQ(0, 0, 0)),
    arma20 = ARIMA(log_return ~ 1 + pdq(2, 0, 0) + PDQ(0, 0, 0)),
    arma30 = ARIMA(log_return ~ 1 + pdq(3, 0, 0) + PDQ(0, 0, 0)),
    arma40 = ARIMA(log_return ~ 1 + pdq(4, 0, 0) + PDQ(0, 0, 0)),
    arma01 = ARIMA(log_return ~ 1 + pdq(0, 0, 1) + PDQ(0, 0, 0)),
    arma11 = ARIMA(log_return ~ 1 + pdq(1, 0, 1) + PDQ(0, 0, 0)),
    arma21 = ARIMA(log_return ~ 1 + pdq(2, 0, 1) + PDQ(0, 0, 0)),
    arma31 = ARIMA(log_return ~ 1 + pdq(3, 0, 1) + PDQ(0, 0, 0)),
    arma41 = ARIMA(log_return ~ 1 + pdq(4, 0, 1) + PDQ(0, 0, 0)),
    arma02 = ARIMA(log_return ~ 1 + pdq(0, 0, 2) + PDQ(0, 0, 0)),
    arma12 = ARIMA(log_return ~ 1 + pdq(1, 0, 2) + PDQ(0, 0, 0)),
    arma22 = ARIMA(log_return ~ 1 + pdq(2, 0, 2) + PDQ(0, 0, 0)),
    arma32 = ARIMA(log_return ~ 1 + pdq(3, 0, 2) + PDQ(0, 0, 0)),
    arma42 = ARIMA(log_return ~ 1 + pdq(4, 0, 2) + PDQ(0, 0, 0)),
    arma03 = ARIMA(log_return ~ 1 + pdq(0, 0, 3) + PDQ(0, 0, 0)),
    arma13 = ARIMA(log_return ~ 1 + pdq(1, 0, 3) + PDQ(0, 0, 0)),
    arma23 = ARIMA(log_return ~ 1 + pdq(2, 0, 3) + PDQ(0, 0, 0)),
    arma33 = ARIMA(log_return ~ 1 + pdq(3, 0, 3) + PDQ(0, 0, 0)),
    arma43 = ARIMA(log_return ~ 1 + pdq(4, 0, 3) + PDQ(0, 0, 0)),
    arma04 = ARIMA(log_return ~ 1 + pdq(0, 0, 4) + PDQ(0, 0, 0)),
    arma14 = ARIMA(log_return ~ 1 + pdq(1, 0, 4) + PDQ(0, 0, 0)),
    arma24 = ARIMA(log_return ~ 1 + pdq(2, 0, 4) + PDQ(0, 0, 0)),
    arma34 = ARIMA(log_return ~ 1 + pdq(3, 0, 4) + PDQ(0, 0, 0)),
    arma44 = ARIMA(log_return ~ 1 + pdq(4, 0, 4) + PDQ(0, 0, 0))
  )

glance(arma)
glance(arma)[which.min(glance(arma)[["AIC"]]), ] # arma 23
glance(arma)[which.min(glance(arma)[["BIC"]]), ] # arma 01

# d= 0 means it is stationary. and no trend.
# d = 0 as we are not using the integrated part of the arima on using AR and MA (AR,I,MA) = (p, d, q)
# If d=0, this means that our data does not tend to go up/down in the long term (i.e., the model is already stationary).
# 
# 


########Question 9   model recommended by aic and perform a test for residual to 20 lags. Interpret the results

arma %>%
  select(arma23) %>%
  report() #  summary() 

a <- arma %>%
  residuals() %>% # get the residuals from arma23
  group_by(.model) %>% # groups the residuals by model
  features(features = ljung_box, lag = 20) # Ljung-Box test for autocorrelation

a[14,] # Reject at 5 % lvl as p_val>0.05. auto correlation exist. cant use this model.

########Question 10  FC (FC = forecast) log return period of May 2020-May 2021. Measure the accuracy formally, graphically. Comment results.

head(data_df) # to find series start.

data_20.21 <- data_df %>% # stop in 2020 MAy for out-of-sample forecasts
  filter(day >= yearmonth("1968-04")) %>% # beginning of TS
  filter(day <= yearmonth("2020-05")) # end of TS

arma_20.21 <- data_20.21 %>% # possible combinations of AR(p) and MA(q) orders
  model(
    arma00 = ARIMA(log_return ~ 1 + pdq(0, 0, 0) + PDQ(0, 0, 0)),
    arma10 = ARIMA(log_return ~ 1 + pdq(1, 0, 0) + PDQ(0, 0, 0)),
    arma20 = ARIMA(log_return ~ 1 + pdq(2, 0, 0) + PDQ(0, 0, 0)),
    arma30 = ARIMA(log_return ~ 1 + pdq(3, 0, 0) + PDQ(0, 0, 0)),
    arma40 = ARIMA(log_return ~ 1 + pdq(4, 0, 0) + PDQ(0, 0, 0)),
    arma01 = ARIMA(log_return ~ 1 + pdq(0, 0, 1) + PDQ(0, 0, 0)),
    arma11 = ARIMA(log_return ~ 1 + pdq(1, 0, 1) + PDQ(0, 0, 0)),
    arma21 = ARIMA(log_return ~ 1 + pdq(2, 0, 1) + PDQ(0, 0, 0)),
    arma31 = ARIMA(log_return ~ 1 + pdq(3, 0, 1) + PDQ(0, 0, 0)),
    arma41 = ARIMA(log_return ~ 1 + pdq(4, 0, 1) + PDQ(0, 0, 0)),
    arma02 = ARIMA(log_return ~ 1 + pdq(0, 0, 2) + PDQ(0, 0, 0)),
    arma12 = ARIMA(log_return ~ 1 + pdq(1, 0, 2) + PDQ(0, 0, 0)),
    arma22 = ARIMA(log_return ~ 1 + pdq(2, 0, 2) + PDQ(0, 0, 0)),
    arma32 = ARIMA(log_return ~ 1 + pdq(3, 0, 2) + PDQ(0, 0, 0)),
    arma42 = ARIMA(log_return ~ 1 + pdq(4, 0, 2) + PDQ(0, 0, 0)),
    arma03 = ARIMA(log_return ~ 1 + pdq(0, 0, 3) + PDQ(0, 0, 0)),
    arma13 = ARIMA(log_return ~ 1 + pdq(1, 0, 3) + PDQ(0, 0, 0)),
    arma23 = ARIMA(log_return ~ 1 + pdq(2, 0, 3) + PDQ(0, 0, 0)),
    arma33 = ARIMA(log_return ~ 1 + pdq(3, 0, 3) + PDQ(0, 0, 0)),
    arma43 = ARIMA(log_return ~ 1 + pdq(4, 0, 3) + PDQ(0, 0, 0)),
    arma04 = ARIMA(log_return ~ 1 + pdq(0, 0, 4) + PDQ(0, 0, 0)),
    arma14 = ARIMA(log_return ~ 1 + pdq(1, 0, 4) + PDQ(0, 0, 0)),
    arma24 = ARIMA(log_return ~ 1 + pdq(2, 0, 4) + PDQ(0, 0, 0)),
    arma34 = ARIMA(log_return ~ 1 + pdq(3, 0, 4) + PDQ(0, 0, 0)),
    arma44 = ARIMA(log_return ~ 1 + pdq(4, 0, 4) + PDQ(0, 0, 0))
  )

glance(arma)
glance(arma)[which.min(glance(arma)[["AIC"]]), ] # minimizes 


FC_arima23 <- data_20.21 %>%
  model(ARIMA(log_return ~ 1 + pdq(2, 0, 3) + PDQ(0, 0, 0))) %>%
  forecast(h = 12, level = 95) # future prediction of a TS from the fitted model, 12 periods ahead

accuracy(FC_arima23, data_df) # evaluation of the FC model with of descriptive measures

# Naive forecast
FC_naive <- data_20.21 %>%
  model(NAIVE(log_return)) %>%
  forecast(h = 12)

accuracy(FC_naive, data_df) # accuracy Naive FC.

p1 <- autoplot(FC_arima23, slice(data_df, (n() - 20):n())) + # autoplot is a wrapper for many ...
  xlab("") + ylab("ARIMA(0,0,3)") # ... objects which saves time in extracting the relevant vectors ... 
# ... out of a specific object

p2 <- autoplot(FC_naive, slice(data_df, (n() - 20):n())) + # "slice() the tail()"
  xlab("") + ylab("Naive")

p <- grid.arrange(p1, p2, ncol = 1, nrow = 2) # combine multiple plots in one
plot(p)

FC_naive   # final observation of last month.
FC_arima23 #  Has better MAE and RMSE than Naive. Value of lag (2) and shock (3) periods both are carried over.
# as lower the MAE and RMSE the better it is.
#
# END. -  1 



############# Work 2 -------------------------------------------------

rm(list = ls())

library(fable)
library(quantmod)
library(PerformanceAnalytics)
library(tseries)
library(ggplot2)
library(dplyr)
library(Quandl)
library(fable)
library(feasts)
library(tsibble)
library(urca)
library(broom)
library(gridExtra)
library(readxl)
library("ggplot2")
library("magrittr") 
library("tidyverse")
library("tsibble")
library("lubridate")
library("quantmod")
library("moments")  
library("fable")
library("feasts")
library("rugarch")
library("cowplot")


############################ Q 1  Volatility Modeling

##--------------------------------------- 1. Extract and plot.


data <- read_excel("Assignment2.xlsx")

head(data)

data <- data %>%
  mutate(return = difference(log(Close), 5) * 100) %>% # create a log-return variable
  tail(-1) %>%
  arrange(Date) %>%
  mutate(data = row_number(Date)) %>%
  as_tsibble(index = data)

data <- na.omit(data) #so no further problems in calculations...

# ploting graph 
p <- data %>%
  ggplot() +
  geom_line(aes(Date, Close), size = 1)
p

p <- data %>%
  ggplot(aes(Date, return)) +
  geom_line(aes(Date, return), size = 1)
p

##--------------------------------------- 2. AIC and ac tests. find Arma garch specif for lof returns. Normal and T-distributed resids.

summary(data$return)

summary(ur.df(data$return, selectlags = "AIC", type ="trend"))  #cant rejct Ho

arma <- data %>% # possible combinations of AR(p) and MA(q) orders (require up to 5,5 but just added extra.)
  model(
    
    arma05 = ARIMA(return ~ 1 + pdq(0, 0, 5) + PDQ(0, 0, 0)),
    arma51 = ARIMA(return ~ 1 + pdq(1, 0, 5) + PDQ(0, 0, 0)),
    arma52 = ARIMA(return ~ 1 + pdq(2, 0, 5) + PDQ(0, 0, 0)),
    arma53 = ARIMA(return ~ 1 + pdq(3, 0, 5) + PDQ(0, 0, 0)),
    arma54 = ARIMA(return ~ 1 + pdq(4, 0, 5) + PDQ(0, 0, 0)),
    arma55 = ARIMA(return ~ 1 + pdq(5, 0, 5) + PDQ(0, 0, 0)),
    
  )

glance(arma)
min_aram <- glance(arma)[which.min(glance(arma)[["AIC"]]), ] # prints min AIC arma model.
min_aram <- min_aram[[".model"]]
min_aram # prints lowest value of AiC arma model to use.

arma %>%
  select(arma52) %>%
  fabletools::report() 

min_data <- data %>% #' ARMA model arma 52 for me.
  model(ar52 = ARIMA(return ~ 1 + pdq(5, 0, 2) + PDQ(0, 0, 0)))


p <- min_data %>% #  ACF of residuals
  select(ar52) %>% # 
  residuals() %>%
  select(.resid) %>%
  feasts::ACF(lag_max = 20) %>% # for visual perception used more lags.
  autoplot() +
  xlab("Lags") +
  ylab("ACF") 
p

p <- min_data %>%   # ACF of residuals^2
  select(ar52) %>%
  residuals() %>%
  mutate(.resid2 = (.resid)^2) %>%
  select(.resid2) %>%
  feasts::ACF(lag_max = 20) %>% # See till 5 lag also highly significant
  autoplot() +
  xlab("Lags") +
  ylab("ACF") 
p # highly significant autocorrelation b/w the squares of residuals


#----- arch and garch specification normal distrib. ARMA 52 order  and GARCH order 50

specARCH <- ugarchspec(                            
  mean.model         = list(armaOrder = c(5, 2)),
  variance.model     = list(model = "sGARCH", garchOrder = c(5, 0)),
  distribution.model = "norm")

ARCH_norm <- ugarchfit(specARCH, data$return) 
ARCH_norm

sum(ARCH_norm@fit$matcoef[10:14, 1])  # sum of coefficients all alphas. is < 1

resid_ARCH <- stats::resid(ARCH_norm@fit)
con_var_ARCH <- c(ARCH_norm@fit$var)
resid_stand_ARCH <- resid_ARCH/I(con_var_ARCH^.5)

kurtosis(resid_stand_ARCH) # 4.7 > 3 so has fat tails or has excess kurtosis.

Box.test(resid_stand_ARCH, lag = 10, type = "Ljung-Box") # no autocorr. in the 1st moment

Box.test(resid_stand_ARCH^2, lag = 10, type = "Ljung-Box")  # No auto corr

#----- arch and garch specification std t-distrib.

specARCH_1 <- ugarchspec(                            
  mean.model         = list(armaOrder = c(5, 2)),
  variance.model     = list(model = "sGARCH", garchOrder = c(5, 0)),
  distribution.model = "std")


ARCH_std <- ugarchfit(specARCH_1, data$return) 
ARCH_std

sum(ARCH_std@fit$matcoef[10:14, 1])  # sum of coefficients all alphas. < 1

resid_ARCH <- stats::resid(ARCH_std@fit)
con_var_ARCH <- c(ARCH_std@fit$var)
resid_stand_ARCH <- resid_ARCH/I(con_var_ARCH^.5)

kurtosis(resid_stand_ARCH) # # also has fat tails and excess kurtosis.

Box.test(resid_stand_ARCH, lag = 10, type = "Ljung-Box") # no autocorr. in the 1st moment

# Test for Condit Heteroskedasticity
Box.test(resid_stand_ARCH^2, lag = 10, type = "Ljung-Box")  # No auto corr
# no higher order arch effect left

##--------------------------------------- 3. Does your result support the premise of volatility clustering?

# Norm distrib and t-distrib
# all Alpha terms are significant
# mu ar1 and ar4 terms are non-significant
# persistance in conditional variance equation
# shows volatility cluster effect or arch effects (kurtosis value)

##--------------------------------------- 4. GJR-GARCH(1,1) and check for leverage effect

specARCH1 <- ugarchspec(                            
  mean.model         = list(armaOrder = c(5, 2)),
  variance.model     = list(model = "gjrGARCH", garchOrder = c(1, 1)),
  distribution.model = "norm")


GJR_ARCH <- ugarchfit(specARCH1, data$return) 
GJR_ARCH

# is gamma > 0 if yes leverage effect exist. if not then no leverage effect

if((GJR_ARCH@fit$matcoef[12, 1]) > 0){
  print("Leverage effect exists")
}else{
  print("Leverage effect does not exist")
}

##--------------------------------------- 5.   EGARCH(1,1) what inferred?

specEARCH1 <- ugarchspec(
  mean.model = list(armaOrder = c(5, 2)),
  variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
  distribution.model = "norm"
)

E_GARCH <- ugarchfit(specEARCH1, data$return)
E_GARCH

# omega constant is +ve and significant.
# Beta1 is big but less than 1 therefore high degree of persistance ~97% and has stationary conditional variance.
# alpha1 and gamma1 values
# 0.102486   0.137439
# aplha1+gamma1 = +ve shock and -aplha1+gamma1 = -ve shock. (arch effects or leverage effects)
# 0.102486 + 0.137439 = 0.239925 23% of shock enters this periods standardized residual condit varince.
# -0.102486 + 0.137439 = 0.034953 3% of shock enters this periods standardized residual condit varince.

############################ Q 2  Downside Risk Modeling

##--------------------------------------- 1. empirical distribution function

head(data)

plot(ecdf(data$return)) # plotting ecdf of returns.

data<- data[,-c(4)] # removes trading day column.

##--------------------------------------- 2. whole sample empirical VaR and ES @ 99% lvl



##--------------------------------------- 3. VaR and ES @ 99% lvl. Historical Simulation. Previous 250 Days.

#made only 99% ones did not make 99.5% and 95% ones.

VaR01 <- rep(NA, dim(data)[1]) # create blank values 

data <- as.data.frame(data) # to run below code converted to data frame.

for(i in 1001:(dim(data)[1])){ # construct quantiles
  
  VaR01[i] <- quantile(data[((i-250):(i-1)), "return"], c(0.01)) #Var 99%
  
}

spec1 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                    mean.model=list(armaOrder=c(5,2), include.mean = TRUE), 
                    distribution="norm") 

roll1 <- ugarchroll(spec1, data[,"return"], n.start=1000, refit.window = "moving", 
                    VaR.alpha = c( 0.01))

spec2 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                    mean.model=list(armaOrder=c(5,2), include.mean = TRUE), 
                    distribution="sstd")

roll2 <- ugarchroll(spec2, data[, "return"], n.start = 1000, refit.window = "moving", 
                    VaR.alpha = c(0.01)) 

spec3 <- ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)), 
                    mean.model=list(armaOrder=c(5,2), include.mean = TRUE), 
                    distribution="norm")

roll3 <- ugarchroll(spec3, data[,"return"], n.start=1000, refit.window = "moving", 
                    VaR.alpha = c(0.01))

spec4 <- ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)), 
                    mean.model=list(armaOrder=c(5,2), include.mean = TRUE), 
                    distribution="sstd")

roll4 <- ugarchroll(spec4, data[,"return"], n.start = 1000, refit.window = "moving", 
                    VaR.alpha = c(0.01))


VaR12 <- c(rep(NA, 1000),roll1@forecast$VaR[,2]) #

VaR22 <- c(rep(NA, 1000),roll2@forecast$VaR[,2]) #

VaR32 <- c(rep(NA, 1000),roll3@forecast$VaR[,2]) #

VaR42 <- c(rep(NA, 1000),roll4@forecast$VaR[,2]) #

VaR99 <- cbind(data, VaR01, VaR12, VaR22, VaR32, VaR42) 

##--------------------------------------- 4. Plot the log-returns together with the computed VaR and ES series in a time series graph

VaR99 <- ts(VaR99[-(1:1000),], start = c(2004, 02, 04), frequency = 252)
plot(VaR99[,c(3,4,7,8)], plot.type="single", col=1:4, ylab=NA, xlab =NA, main="VAR(99%)")
legend(x=2004, y=-14.5, legend=c("Return","Historical Simulation", 
                                 "Normal ARMA(5,2)-GJR-GARCH(1,1)", 
                                 "Student t ARMA(5,1)-GJR-GARCH(1,1)"), 
       lty= c(1,1), col=1:4, bty="n")

##--------------------------------------- 5. Back-test your VaR estimates for violation independence as well as correct conditional and unconditional coverage.

BT99Tab <- matrix(NA,5,3) # empty box creation table for back test

for(i in 4:8){ # structured summary creation
  
  BT <- BacktestVaR(VaR99[,3],VaR99[,i],0.01)
  BT99Tab[(i-3),1] <- BT$AE               # Actual over Expected exceedence ratio
  BT99Tab[(i-3),2] <- BT$LRuc[2]          # Unconditional coverage
  BT99Tab[(i-3),3] <- BT$LRcc[2]          # conditional coverage
  
}

head(VaR99)

#remanimg rows and cols for our ease. 

colnames(BT99Tab) <- c("theta", "UC", "CC")

rownames(BT99Tab) <- c("Historical Simulation", 
                       "Normal ARMA(5,1)-GARCH(1,1)", 
                       "Student-t ARMA(5,1)-GARCH(1,1)", 
                       "Normal ARMA(5,1)-GJR-GARCH(1,1)", 
                       "Student-t ARMA(5,1)-GJR-GARCH(1,1)")

stargazer::stargazer(BT99Tab, digits=4)

BT99Tab
