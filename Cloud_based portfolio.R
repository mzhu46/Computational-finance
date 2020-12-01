# import library
library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
require(ROI)
require(ROI.plugin.glpk)
require(ROI.plugin.quadprog)


# # generate return.df ---------------------------------------
# and turn it to suitable df form
return = read.csv("C:/706 computational Finance/Project/returns.csv")

return.long=return[,c("datadate","tic","trt1m")]


return.long$date=strptime(return.long$datadate, format = "%Y%m%d")

return.long = return.long[,c("date","tic","trt1m")]

library(tidyr)
return.wide = pivot_wider(return.long, names_from = tic , values_from = trt1m)
#deleting the last empty row
return.df = return.wide[1:nrow(return.wide)-1,]

# cloud_tickers -----------------------------------------------------------


cloud_tickers = c("CRM","AMAZ","MSFT","ADBE","TWLO","SPLK","AMD","NFLX","NVDA")


t1=intersect(cloud_tickers,colnames(return.df))
Record_cloud = c()
for (i in seq(13,length(index(return.df)),12)){
  
  if(i == 121){
    portfolio.ticker=t1
    temp1 = Op_track(portfolio.ticker,i,12,9,2000)
  }else{
    portfolio.ticker=t1
    temp1 = Op_track(portfolio.ticker,i,12,12,1000)}
  
  Record_cloud = rbind(Record_cloud,temp1)}

cumprod(Record_cloud$`test portfolio`+1)


Record_withcloud = Record
Record_withcloud$balance_cloud=0;
Record_withcloud$balance_cloud =cumprod(Record_withcloud$`test portfolio`+1)


# Value at Risk -----------------------------------------------------------

# regenerate the most recent portfolio and study its weights and 

Op_track_weights=function(portfolio.ticker1,start_month, len_optimize,len_track,random_search,box_upper){
  #data clean to locate a clean df1.withoutna
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
  
  return(y_optimal)
  
  
}

y=2018;i=121
S1 = sort_by_npm(y,25,df)
portfolio.ticker=Delete_SM(S1,Social_Media_Tickers)
temp1 = Op_track(portfolio.ticker,i,12,9,20000,0.07)
temp2 = Op_track_weights(portfolio.ticker,i,12,9,20000,0.07)

sort(temp2$weights)
#take the barplot to the detailed holding position for those weight > 2%
barplot(sort(temp2$weights)[7:length(sort(temp2$weights))])


constituents_list=names(sort(temp2$weights))

#select these stocks return

return_constituents=return.df[constituents_list]/100

result1=sapply(return_constituents, function(x) c(mean=mean(x), var=var(x), sd=sd(x)))

check_volatile = function (df,tickers , start_month,len_track){
  df1 = df[tickers]/100
  df2 = df1[start_month:(start_month+len_track),]
  
  result1=sapply(df2, function(x) c(mean=mean(x), var=var(x), sd=sd(x)))
  return(result1)
}

sort(check_volatile(return.df,constituents_list,121,9)[3,])

# focus on the most recent 9 month data, the most volatile stocks are FLT, AAPL, ANET, IPGP, MA as top5












# compare with index ------------------------------------------------------
#^IRX is 13 Weekly treasury bill which can be used as risk free rate and that can be used to calculate beta in CAPM model
# get data
ticker_list = c("^IXIC","^GSPC","^IRX")

stock_prices <- NULL

for (ticker in ticker_list){
  stock_prices <- cbind(stock_prices, getSymbols.yahoo(ticker, 
                                                       from = "2011-01-01", periodicity= 'monthly', auto.assign=FALSE)[,4])
}


s1=stock_prices
stock_prices$IXIC.Close = stock_prices$IXIC.Close/as.numeric(stock_prices$IXIC.Close[1])
stock_prices$GSPC.Close = stock_prices$GSPC.Close/as.numeric(stock_prices$GSPC.Close[1])

Record_index = stock_prices[1:(length(index(Record)))]
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

var(Record2)

plot(Record2)

Record_beta

