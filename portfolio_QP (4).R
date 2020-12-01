

library(ggplot2)
library(quadprog)
library(quantmod)

# get data
ticker_list = c("ECL", "CF", "SHW",
                "APD", "IFF",	"PPG", "FMC",
                "DD",	"CE",	"ALB")

stock_prices <- NULL

for (ticker in ticker_list){
  stock_prices <- cbind(stock_prices, getSymbols.yahoo(ticker, 
                                                       from = '2010-01-03', periodicity= 'weekly', auto.assign=FALSE)[,4])
}


stock_returns = na.omit(ROC(stock_prices))
colnames(stock_returns) = ticker_list

stock_returnx = as.data.frame(stock_returns)
Retn= colMeans(stock_returnx)
Risk = diag(var(stock_returnx))
RiskReturn = as.data.frame(t(rbind(Retn, Risk)))
plot1 <- ggplot(data = RiskReturn, aes(x=Risk, y=Retn))
plot1 <- plot1+geom_point()
plot1 <- plot1+xlab("Variance") + ylab("weekly returns") +
  ggtitle("risk/Return")

plot1

# Quadratic Programming 
#solve.QP(Dmat, dvec, Amat, bvec, meq=0, factorized=FALSE)
#
#Arguments
#---Dmat: matrix appearing in the quadratic function to be minimized.
#---dvec: vector appearing in the quadratic function to be minimized.
#---Amat: matrix defining the constraints under which we want to minimize the quadraticfunction.
#---bvec: vector holding the values of b0 (defaults to zero).
#---meq: the first meq constraints are treated as equality constraints, all further as inequality constraints (defaults to 0).
#  factorized logical flag: if TRUE, then we are passing R???1
#(where D = RT R) instead of the matrix D in the argument Dmat


asset_returns = stock_returnx  # stock returns as a data frame 

PortRet = 0.005 # Expected Portfolio Return in data frequency (daily, weekly etc.)

# Inputs for Solve.QP

PQ_Solve = function (asset_returns, PortRet){
  

Dmat = 2*cov(asset_returns)
dvec = rep(0, ncol(asset_returns))
Amat = cbind(rep(1, ncol(asset_returns)), colMeans(asset_returns))
bvec = c(1, PortRet)

# Quadratic Optimization
optim.result <- solve.QP(Dmat=Dmat,
                         dvec=dvec,
                         Amat = Amat,
                         bvec = bvec)

return(optim.result)

}

# How do we generate the Efficient Frontier?
# Try different Target Portfolio Returns and collect the correspondind Variance of the Optimal Portfolio

minRet = 0
maxRet = 0.04

EFFrontier <- function(asset_returns, minRet, maxRet){
  
  smuP <- seq(minRet, maxRet, length=50)
  svarp <- sapply(smuP, function(x){PQ_Solve(asset_returns,x)$value})
  EffF <- as.data.frame(cbind(smuP,svarp))
  minvar <- min(EffF$svarp)
  L= EffF$svarp==minvar
  minRet = EffF[L,]$smuP
  minPoint = as.data.frame(cbind(minRet, minvar))
  minvarwP= PQ_Solve(asset_returns, minRet)$solution
  rList = list(EffF, minPoint, minvarwP)
  names(rList) <- c("EFF", "minpoint", "wp")
  return(rList)
  
}


z = EFFrontier(asset_returns, 0, 0.04)


plot2 = ggplot(data=z$EFF, aes(x=svarp, y=smuP))
plot2 = plot2+geom_point()
plot2 = plot2 + geom_point(data=z$minpoint, aes(x=minvar, y= minRet, color="red", size=3))
plot2 = plot2+ xlab("Variance") + ylab("Returns") + ggtitle("efficient Frontier")
plot2

