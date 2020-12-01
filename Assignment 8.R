

library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
require(ROI)
require(ROI.plugin.glpk)
require(ROI.plugin.quadprog)

library(sparseIndexTracking)

ticker_list = c("C", "XOM", "SHW",
                "APD", "IFF",	"PPG", "FMC",
                "DD",	"CE",	"AMZN", "ALGN", "ADBE", "YUM", "PPL", "KO", "TXT", "MO", "WAT", "HCA")

stock_prices <- NULL
start_date = '2017-01-03'

for (ticker in ticker_list){
  stock_prices <- cbind(stock_prices, getSymbols.yahoo(ticker, 
                                                       from = start_date, periodicity= 'daily', auto.assign=FALSE)[,4])
}


stock_returns = na.omit(ROC(stock_prices))
colnames(stock_returns) = ticker_list

# collect SP 500 Index data with the same method
# Use: 

SP500_prices = getSymbols.yahoo("^GSPC", 
                                 from = start_date, periodicity= 'daily', auto.assign=FALSE)[,4]

SP500_returns = na.omit(ROC(SP500_prices))

w_ete <- spIndexTrack(stock_returns, SP500_returns, lambda = 1e-7, u = 0.5, measure = 'ete')

# use a bar plot to show how much suitbale weight should be put on each stock to mock the Sp500 index performance

barplot(w_ete)

# to find the optimal index tracking portfolio by using a 2 year window from from '2017-01-03' to '2017-01-03'+2 years (~ 2019-Jan-05)

# Using the data from 2017-01 to 2019-01 ----------------------------------
# slice the original data df
nrow(SP500_returns)
# find the suitable index of 2019-01-05 # there is no 01-05 avtive trading days so find the most close previous one
b1=SP500_returns[1:504,]
a1 = stock_returns[1:504,]
w_2 = spIndexTrack(a1, b1, lambda = 1e-7, u = 0.5, measure = 'ete')

# slice the 2019-01-2020-01 to get w_1 for later comparision

b2=SP500_returns[504:756,]
a2=stock_returns[504:756,]
w_1 = spIndexTrack(a2, b2, lambda = 1e-7, u = 0.5, measure = 'ete')

w_1
w_2
w_ete
library(dplyr)
library(tidyr)
library(ggplot2)

w_combined = rbind(w_2,w_1,w_ete)
w_df = as.data.frame(t(w_combined))


# barplot for the df includes the weights from different time peri --------

library(tidyverse)
w_3 = w_df
w_3["Stocks"] = rownames(w_df)

w_3
dat2 <- w_3 %>%
  gather(Total, Weights, -Stocks)

ggplot(dat2, aes(x = Stocks, y = Weights, fill = Total)) +
  geom_col(position = "dodge")

# w_1 stands for use the most recent 1 year data
# w_2 stands for use the most recent 2 year data
# w_ete stands for use the all available data


# Assume, you keep weights the same all the way to the end of 2020-Oct 
# Calculate the returns of the  "tracking portfolio" 
# What is your tracking error (stdev of daily returns - annualized) in moving 3-month windows in 2020? 
# Present a chart


# Check portfolio perfomance during 2019-01 to 2020-10 --------
# do the comparison based on diff and stdev(Portfolio Returns - Index Returns)

# slice out the suitable range to check 2019-01 to 2020-10
R_test = stock_returns[504:943,]
w_2.return = Return.portfolio(R_test, w_2) 

R_compare = SP500_returns[504:943,]

R_df = cbind(w_2.return$portfolio.returns,R_compare)

plot.xts(R_df)
plot.xts(R_compare)

sd((R_df$portfolio.returns-R_df$GSPC.Close))

length((R_df$portfolio.returns-R_df$GSPC.Close))
#build a series tracking how std of diff change
std_record = c()
for (i in 1:(length((R_df$portfolio.returns-R_df$GSPC.Close))-60)){

  diff1 = (R_df$portfolio.returns-R_df$GSPC.Close)
  s1 = sd(diff1[i:(i+60)])
  if (s1 > 0.005){
    print(index((R_df$portfolio.returns-R_df$GSPC.Close))[i])
  }

  std_record = cbind(std_record,s1)
}


plot(t(std_record))



# build a pipeline to generate tracking error  ----------------------------

# inputs include the portfolio weights and the date range for tracking performance

std_trackerror = function ( port_wei , date_st, date_end){
  
  R_test = stock_returns[date_st:date_end,]
  R_compare = SP500_returns[date_st:date_end,]
  w_2.return = Return.portfolio(R_test, port_wei) 
  
  R_df = cbind(w_2.return$portfolio.returns,R_compare)
  
  std_record = c()
  for (i in 1:(length((R_df$portfolio.returns-R_df$GSPC.Close))-60)){
    
    diff1 = (R_df$portfolio.returns-R_df$GSPC.Close)
    s1 = sd(diff1[i:(i+60)])
    
    std_record = cbind(std_record,s1)
  }
  
  return(t(std_record))
  
}


error_2year=(std_trackerror(w_2,504,943))
length(error_2year)
plot(error_2year)
#the plot shows that the tracking performance at first 200 60-days window perform very well
#things changed a lot since 2019-12, the track error increased quite a lot and constructure a peak during 2019-12 to 2020-05


# Move window to improve the tracking performance -------------------------
# Would you be able to improve the tracking error if you updated the portfolio weights with recent data? 
# Try using moving 6-month windows from [2019-Jun to 2020-Jan] and then updating weights every month. See if your tracking error changes.

# use a for loop to update the weights
# going to use 2018-06 to 2018-12 to optimize a portfolio to track on 2019-01 and move this window monthly
b2=SP500_returns[504:756,]
a2=stock_returns[504:756,]

b_2018_06 = SP500_returns[355:503]
a_2018_06 = stock_returns[355:503,]
w_2019_01 = spIndexTrack(a_2018_06, b_2018_06, lambda = 1e-7, u = 0.5, measure = 'ete')
std_trackerror(w_2,504,(504+30+60))


error_m = c()
for (i in (seq(504,943,by=30))){
  b_ = SP500_returns[(i-180):(i-1)]
  a_ = stock_returns[(i-180):(i-1)]
  w_ = spIndexTrack(a_, b_, lambda = 1e-7, u = 0.5, measure = 'ete')
  
  if (i == 924){
    record_ = std_trackerror(w_,i,943)
    error_m = rbind(error_m,record_)}
    
  else{  
    record_ = std_trackerror(w_,i,(i+30+60))
    error_m = rbind(error_m,record_)}

  
  

}
plot(error_2year)
par(new = TRUE)
plot(error_m, axes = FALSE, xlab = "", ylab = "",col="red")

#w_1 stands for the porfilio constructed by the recent 1 year data 
error_1year=(std_trackerror(w_1,504,943))


#for comparision plot two 



matplot (index(R_df)[61:440], cbind (error_2year, error_1year),col=c("black","green"), pch = 20,xlab = "time", main = " Comparations between 2 index tracking portfolio")
legend("topleft",c("error_2year","error_1year"),col = c("black","green"), lty = 1:2, lwd = 1)


# we can observe that the new portfolio constructed by the most recent one year 
# performs better during the first half period while it shows even worse result during the 2019-12 to 2020-04
# after that tracking error peak, the new portfolio again outperform than the previous one

matplot (index(R_df)[61:440], cbind (error_2year, error_m),col=c("black","green"), pch = 20,xlab = "time", main = " Comparations between 2 index tracking portfolio")
legend("topleft",c("error_2year","error_1year"),col = c("black","green"), lty = 1:2, lwd = 1)
