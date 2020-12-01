#import the necessary library
library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
require(ROI)
require(ROI.plugin.glpk)
require(ROI.plugin.quadprog)


library(readr)
data3 <- read_csv("C:/706 computational Finance/Project/data3.csv")
# the data covers the financial statement whose publication date is from 2010-1 to 2019-12
df = as.data.frame(data3)
# we tend to have access to the financial statement public report date 
#and assume that it reports the last year performance at the following year
df["Year"]=as.numeric(substr(df$public_date,1,4))-1


df_2009=df[df$Year==2009,]
df_2009$TICKER
unique(df$TICKER)
library(dplyr)
df_2009_ticker = group_by(df_2009,TICKER)

#using groupby to take mean of npm and pe of the last year to have a general impression of company's profit earning ability

df_groupby=df_2009_ticker%>% summarise(
  avg_npm = mean(npm),
  avg_pe = (mean(pe_exi)+mean(pe_inc))/2
)
#remove the stocks with negative npm

df_nonegative=df_groupby[df_groupby$avg_npm>0,]
#sort it by the desc of net profit margin and get the top ten

top_10=df_nonegative[order(-df_nonegative$avg_npm),][1:10,1]

top_10=as.list(top_10)

# build a function using the same logic to sort out profitable company based on npm

#year1 is the interested year when company has different financial ratios
#number 1 is how many companies we want to sort out based on npm
sort_by_npm=function(year1,number1,df){
  df_year = df[df$Year==year1,]
  df_year_ticker = group_by(df_year,TICKER)
  
  df_groupby=df_year_ticker%>% summarise(
    avg_npm = mean(npm),
    avg_pe = (mean(pe_exi)+mean(pe_inc))/2
  )
  
  df_nonegative=df_groupby[df_groupby$avg_npm>0,]
  
  top_10=df_nonegative[order(-df_nonegative$avg_npm),][1:number1,1]
  
  top_10=as.list(top_10)
  
  return(top_10)
  
  
}

S1=sort_by_npm(2010,30,df)

S1

# delete the social media stocks ------------------------------------------

Social_Media_Tickers = c("AMAZ","GOOG","GOOGL","TWTR","FB","GRPN","YELP","MOMO","WB","FFIV")


Delete_SM = function(S,tickers){
  s1=S$TICKER
  # loop the ticker list to kick out social media stocks
  for (i in 1:length(tickers)){
    s1=s1[s1 != tickers[i]]
  }
  
  return(s1)
}

portfolio.ticker=Delete_SM(S1,Social_Media_Tickers)


# add the tickers of cloud based companies --------------------------------
# this failed because how to locate these stocks in the basked to form group A and set constrains
# develop a new way that split out 25% of our fund and only put on these cloud based stocks 
cloud_tickers = c("CRM","AMAZ","MSFT","ADBE","TWLO","SPLK","AMD","NFLX","NVDA")
ADD_Cloud = function(S,tickers){
  S = c(S,tickers)
  s1 = unique(S)
  return (s1)
}

intersect(cloud_tickers,colnames(return.df))




portfolio.ticker = ADD_Cloud(portfolio.ticker,cloud_tickers)
portfolio.ticker

# Turn price long df into wide normal df ----------------------------------

price = read.csv("C:/706 computational Finance/Project/price.csv")
colnames(price)
price.long=price[,c("date","TICKER","PRC")]


price.long$date=strptime(price.long$date, format = "%Y%m%d")
library(tidyr)
price.long

price.wide <- pivot_wider(price.long, names_from = TICKER, values_from = PRC ) 


price.wide <- reshape(data=price.long,idvar="TICKER",
                          
                          v.names = "PRC",
                          
                          timevar = "date",
                          
                          direction="wide")

price.df=t(price.wide)
a1=as.Date(substr(rownames(price.df)[2:length(rownames(price.df))],5,14))
typeof(a1)
b1=as.vector(price.wide$TICKER)
b1
price.wide$TICKER
price.df = as.data.frame(price.df, row.names = a1,col.name=b1)

price.df = price.df[2:157,]
colnames(price.df)=b1
rownames(price.df)=a1
price.df
library("zoo") 



plot(price.df[1:10,1:10])


# check the price return --------------------------------------------------
# and turn it to suitable df form
return = read.csv("C:/706 computational Finance/Project/returns.csv")

return.long=return[,c("datadate","tic","trt1m")]


return.long$date=strptime(return.long$datadate, format = "%Y%m%d")

return.long = return.long[,c("date","tic","trt1m")]

library(tidyr)
return.wide = pivot_wider(return.long, names_from = tic , values_from = trt1m)
#deleting the last empty row
return.df = return.wide[1:nrow(return.wide)-1,]

#summary statistics
summary(return.df)
#check the plot of return for the stock 2:5
plot.ts(return.df[,2:5])


colnames(return.df)
unique(df$TICKER)

length(intersect(colnames(return.df),unique(df$TICKER)))

# take the intersect of return.df and our tickers, because some stocks have financial ratios 
#but not price data while someothers have opposite situation

return.df2=return.df[intersect(colnames(return.df),unique(df$TICKER))]

rownames(return.df2)=return.df$date

return.df3=return.df2[intersect(colnames(return.df2),portfolio.ticker)]


return.df2

# Optimize using package --------------------------------------------------

# 111 slice the return.df3
df1=return.df3[1:30,]
df1
fund.names <- colnames(df1)

rownames(df1)=return.df$date[1:10]

df1.withoutna=df1[,colSums(is.na(df1))==0]

df1.withoutna/100
rownames(df1.withoutna) = return.df$date[1:30]

fund.names <- colnames(df1.withoutna)



portf <- portfolio.spec(assets=fund.names) # Create a portfolio object



portf <- add.constraint(portf, type="full_investment")
portf <- add.constraint(portf, type="long_only")

portf.minStdDev <- add.objective(portf, type="risk", name="StdDev")


portf.minStdDevmaxreturn = add.objective(portfolio=portf.minStdDev, type="return", name="mean")

y_optimal = optimize.portfolio(df1.withoutna, portfolio = portf.minStdDevmaxreturn, constraints = NULL,
                              objectives = NULL, optimize_method = c("random"), search_size = 10000, trace = FALSE)

y_optimal$weights

# use portfolio return to get  the performance of our portfolio--------------------------------------------

portf.R = Return.portfolio(df1.withoutna/100,y_optimal$weights)


portf.E=equal.weight(df1.withoutna/100,portf.minStdDevmaxreturn)
portf.E_R = Return.portfolio(df1.withoutna/100, portf.E$weights)
portf.E_R

backtest.df=cbind(portf.R,portf.E_R)
colnames(backtest.df)=c("test portfolio","equal weight bench")

summary(backtest.df)

y_optimal$weights

# build a pipeline to generate the return performance ---------------------

#parameter return.df
#starting and ending month number
# we are going to use the retunr.df2 as our main dataframe for optimizing and return tracking

Op_track=function(portfolio.ticker1,start_month, len_optimize,len_track,random_search,box_upper){
  #data clean to locate a clean df1.withoutna
  return.df3=return.df2[intersect(colnames(return.df2),portfolio.ticker1)]
  
  
  df_op_track=(return.df3[(start_month-len_optimize):(start_month+len_track-1),]/100)
  df_op_track.withoutna = df_op_track[,colSums(is.na(df_op_track))==0]
  
  
  df1 = df_op_track.withoutna[1:len_optimize,]
  rownames(df1)=return.df$date[(start_month-len_optimize):(start_month-1),]
  
  
  fund.names <- colnames(df1)
  portf <- portfolio.spec(assets=fund.names) # Create a portfolio object
  
  #adding constrains
  
  portf <- add.constraint(portf, type="full_investment")
  portf <- add.constraint(portf, type="long_only")
  #add box constrains
  portf <- add.constraint(portf, type="box",min=0,max=box_upper)
  
  # adding target to get minstd and max mean
  portf.minStdDev <- add.objective(portf, type="risk", name="StdDev")
  portf.minStdDevmaxreturn = add.objective(portfolio=portf.minStdDev, type="return", name="mean")
  
  #optimize
  y_optimal = optimize.portfolio(df1, portfolio = portf.minStdDevmaxreturn, constraints = NULL,
                                 objectives = NULL, optimize_method = c("random"), search_size = random_search, trace = FALSE)
  
  
  
  #locate the df2 is the data range to record the portfolio performance
  df2=df_op_track.withoutna[(len_optimize+1):(len_optimize+len_track),]
  
  rownames(df2) =return.df$date[(start_month):(start_month+len_track-1),]
  #use y_optimal$weight to calculate the portfolio performance
  portf.R = Return.portfolio(df2,y_optimal$weights)
  
  #use equal weight portfolio as comparison
  portf.E=equal.weight(df1,portf.minStdDevmaxreturn)
  portf.E_R = Return.portfolio(df2, portf.E$weights)
  #build a new dataframe for return comparision
  backtest.df=cbind(portf.R,portf.E_R)
  colnames(backtest.df)=c("test portfolio","equal weight bench")
  rownames(backtest.df)=return.df$date[(start_month):(start_month+len_track-1),]
  
  return(backtest.df)
  
  
}

Op_track(portfolio.ticker,20,10,40,1000,0.07)

a1=Op_track(portfolio.ticker,20,15,6,1000)
a1









# test --------------------------------------------------------------------

portfolio.ticker1=portfolio.ticker
start_month=20;len_optimize=10;len_track=40


return.df3=return.df2[intersect(colnames(return.df2),portfolio.ticker1)]


df_op_track=(return.df3[(start_month-len_optimize):(start_month+len_track-1),]/100)
df_op_track.withoutna = df_op_track[,colSums(is.na(df_op_track))==0]


df1 = df_op_track.withoutna[1:len_optimize,]
rownames(df1)=return.df$date[(start_month-len_optimize):(start_month-1),]

df1

fund.names <- colnames(df1)
portf <- portfolio.spec(assets=fund.names) # Create a portfolio object

#adding constrains

portf <- add.constraint(portf, type="full_investment")
portf <- add.constraint(portf, type="long_only")
portf <- add.constraint(portf, type="box",min=0,max=0.07)

# adding target to get minstd and max mean
portf.minStdDev <- add.objective(portf, type="risk", name="StdDev")
portf.minStdDevmaxreturn = add.objective(portfolio=portf.minStdDev, type="return", name="mean")

#optimize
y_optimal = optimize.portfolio(df1, portfolio = portf.minStdDevmaxreturn, constraints = NULL,
                               objectives = NULL, optimize_method = c("random"), search_size = 1000, trace = FALSE)

y_optimal

#locate the df2 is the data range to record the portfolio performance
df2=df_op_track.withoutna[(len_optimize+1):(len_optimize+len_track),]

rownames(df2) =return.df$date[(start_month):(start_month+len_track-1),]
#use y_optimal$weight to calculate the portfolio performance
portf.R = Return.portfolio(df2,y_optimal$weights)

#use equal weight portfolio as comparison
portf.E=equal.weight(df1,portf.minStdDevmaxreturn)
portf.E_R = Return.portfolio(df2, portf.E$weights)
#build a new dataframe for return comparision
backtest.df=cbind(portf.R,portf.E_R)
colnames(backtest.df)=c("test portfolio","equal weight bench")
rownames(backtest.df)=return.df$date[(start_month):(start_month+len_track-1),]


summary(backtest.df)


# ye ----------------------------------------------------------------------

# use a for loop to do portfolio rebalancing
# optimize the portfolio use the all available data

index(return.df)

seq(13,length(index(return.df)),12)
Record = c()

for (i in seq(13,length(index(return.df)),12)){
  
  if(i == 121){
    y=2018
    S1 = sort_by_npm(y,25,df)
    portfolio.ticker=Delete_SM(S1,Social_Media_Tickers)
    set.seed(499)
    temp1 = Op_track(portfolio.ticker,i,12,9,20000,0.5)
    last_year_ticker = portfolio.ticker
  }else{
    y=(i%/%12+2009)
    S1 = sort_by_npm(y,25,df)
    portfolio.ticker=Delete_SM(S1,Social_Media_Tickers)
    temp1 = Op_track(portfolio.ticker,i,12,12,20000,0.5)}
  
  Record = rbind(Record,temp1)}


Record$balance1=0;Record$balance_eq=0;
Record$balance1 =cumprod(Record$test.portfolio+1)
Record$balance_eq=cumprod(Record$equal.weight.bench+1)

Record

plot(cbind(Record$balance1,Record$balance_eq),main = "use 12 months as optimization window \n hold 12 months till next rebalance date")


# compare with index ------------------------------------------------------
#^IRX is 13 Weekly treasury bill which can be used as risk free rate and that can be used to calculate beta in CAPM model
# get data
ticker_list = c("^IXIC","^GSPC","^IRX")

stock_prices <- NULL

for (ticker in ticker_list){
  stock_prices <- cbind(stock_prices, getSymbols.yahoo(ticker, 
                                                       from = '2011-01-01', periodicity= 'monthly', auto.assign=FALSE)[,4])
}


s1=stock_prices
stock_prices$IXIC.Close = stock_prices$IXIC.Close/as.numeric(stock_prices$IXIC.Close[1])
stock_prices$GSPC.Close = stock_prices$GSPC.Close/as.numeric(stock_prices$GSPC.Close[1])

Record_index = stock_prices[1:length(index(Record))]
index(Record_index) = index(Record)

Record2 = cbind(Record$balance1,Record_index$IXIC.Close)
plot(Record2,main = "optimal portfolio based on npm \n against the Nasdq index")
# test loop 2 -------------------------------------------------------------


Op_track(portfolio.ticker,13,7,12,1000)  
  
for (i in seq(13,length(index(return.df)),12)){
  print(i%/%12+2009)
  y=(i%/%12+2009)
  print(sort_by_npm(y,20,df))
  print(Op_track(portfolio.ticker,i,12,12,1000))
}

y=2018
portfolio.ticker = sort_by_npm(y,25,df)


sort_by_npm(2018,25,df)


# Calculate the beta of the portfolio -------------------------------------


# use arithmetic return instead of log return 
#reasoning Log returns are not used because the theoretical rationale for such models (e.g. APT)
#revolves around expected portfolio returns not expected portfolio log returns 
#(e.g. the difference in log returns is the log of a ratio involving returns).
Delt(Record2$balance1,type = "arithmetic")


#turn the annulized to monthly
Record_beta = cbind(Delt(Record2$balance1,type = "arithmetic"),Delt(Record2$IXIC.Close,type = "arithmetic"),Record_index$IRX.Close/1200)
Record_beta = Record_beta[2:length(index(Record_beta))]
Record_beta
CAPM_model = lm((Record_beta$Delt.1.arithmetic-Record_beta$IRX.Close)~(Record_beta$Delt.1.arithmetic.1-Record_beta$IRX.Close))
summary(CAPM_model)

#the beta is estimated to be 0.99 with 0.04 std 

CAPM.beta((Record_beta$Delt.1.arithmetic-Record_beta$IRX.Close),(Record_beta$Delt.1.arithmetic.1-Record_beta$IRX.Close))



# VaR part ----------------------------------------------------------------


Op_construct=function(portfolio.ticker1,start_month, len_optimize,len_track,random_search,box_upper){
  #data clean to locate a clean df1.withoutna
  return.df3=return.df2[intersect(colnames(return.df2),portfolio.ticker1)]
  
  
  df_op_track=(return.df3[(start_month-len_optimize):(start_month+len_track-1),]/100)
  df_op_track.withoutna = df_op_track[,colSums(is.na(df_op_track))==0]
  
  
  df1 = df_op_track.withoutna[1:len_optimize,]
  rownames(df1)=return.df$date[(start_month-len_optimize):(start_month-1),]
  
  fund.names <- colnames(df1)
  portf <- portfolio.spec(assets=fund.names) # Create a portfolio object
  
  #adding constrains
  
  portf <- add.constraint(portf, type="full_investment")
  portf <- add.constraint(portf, type="long_only")
  #add box constrains
  portf <- add.constraint(portf, type="box",min=0,max=box_upper)
  
  # adding target to get minstd and max mean
  portf.minStdDev <- add.objective(portf, type="risk", name="StdDev")
  portf.minStdDevmaxreturn = add.objective(portfolio=portf.minStdDev, type="return", name="mean")
  
  #optimize
  y_optimal = optimize.portfolio(df1, portfolio = portf.minStdDevmaxreturn, constraints = NULL,
                                 objectives = NULL, optimize_method = c("random"), search_size = random_search, trace = FALSE)
  
  
  return(y_optimal)
  
  
}
Op_construct=function(portfolio.ticker1,start_month, len_optimize,len_track,random_search,box_upper){
  #data clean to locate a clean df1.withoutna
  return.df3=return.df2[intersect(colnames(return.df2),portfolio.ticker1)]
  
  
  df_op_track=(return.df3[(start_month-len_optimize):(start_month+len_track-1),]/100)
  df_op_track.withoutna = df_op_track[,colSums(is.na(df_op_track))==0]
  
  
  df1 = df_op_track.withoutna[1:len_optimize,]
  rownames(df1)=return.df$date[(start_month-len_optimize):(start_month-1),]
  
  return(df1)
  
  
}
set.seed(499)
last_year_portfolio=Op_construct(last_year_ticker,121,12,9,20000,0.5)

last_year_return
