  
  library(quantmod)
  library(PerformanceAnalytics)
  library(PortfolioAnalytics)
  require(ROI)
  require(ROI.plugin.glpk)
  require(ROI.plugin.quadprog)
  
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
  
  
  portf <- portfolio.spec(colnames(stock_returns))
  portf
  portf <- add.constraint(portf, type="weight_sum", min_sum=0.99, max_sum=1)
  portf
  
  portf <- add.constraint(portf, type="box", min =0, max =0.25)
  portf
  
  portf <- add.objective(portf, type="return", name="mean")
  portf <- add.objective(portf, type="risk", name="var")
  
  
  opt_Portf = optimize.portfolio(stock_returns, portf, optimize_method="random", 
                                 search_size = 1000)
  
  opt_Portf = optimize.portfolio(stock_returns, portf, optimize_method="ROI")
  
  opt_Portf = optimize.portfolio(stock_returns, portf, optimize_method="ROI", trace=TRUE)
  
  
  # look at ROI package
  
  chart.Weights(opt_Portf)
  efrontier = extractEfficientFrontier(opt_Portf, 
                                       match.col="StdDev", n.portfolios = 25, risk_aversion = NULL)
  chart.EfficientFrontier(efrontier, 
                          match.col = "StdDev", n.portfolios = 25, xlim = NULL, ylim = NULL,
                          cex.axis = 0.8, element.color = "darkgray", main = "Efficient Frontier",
                          RAR.text = "SR", rf = 0.03/52, tangent.line = TRUE, cex.legend = 0.8,
                          chart.assets = TRUE, labels.assets = TRUE, pch.assets = 21,
                          cex.assets = 0.8)
  
  
  portf <- portfolio.spec(colnames(stock_returns))
  portf <- add.constraint(portf, type="weight_sum", min_sum=0.99, max_sum=1)
  
  portf <- add.constraint(portf, type="box", min =0, max =0.25)
  portf <- add.objective(portf, type="return", name="mean")
  portf <- add.objective(portf, type="risk", name="StdDev", target =0.005)
  
  opt_Portf = optimize.portfolio(stock_returns, portf, optimize_method="random", 
                                 search_size = 10000)
  
  rp = random_portfolios(portf, 100, "sample")
  
  opt_rebal =optimize.portfolio.rebalancing(stock_returns, portf,
                                            optimize_method = "random", rp=rp,
                                            rebalance_on="quarters", 
                                            training_period=3, 
                                            rolling_window = 6)
  
  
  eqw_p = rep(1/ncol(stock_returns), ncol(stock_returns))
  benchmark = Return.portfolio((stock_returns), weights = eqw_p)
  colnames(benchmark) = "Benchmark"
  SP500_index <- getSymbols.yahoo("SPY", 
                                  from = '2010-01-03', periodicity= 'weekly', auto.assign=FALSE)[,4]
  
  SP500Rets = na.omit(ROC(SP500_index))
  SP500Rets = as.xts(SP500Rets)
  
  
  chart.Weights(opt_rebal, main="Weights over Time")
  
  rebal_weights <- extractWeights(opt_rebal)
  
  rebal_returns <- Return.portfolio(stock_returns, weights=rebal_weights)
  rets_df = cbind(rebal_returns, benchmark, SP500Rets)
  
  charts.PerformanceSummary(rets_df, main ="Performance")

  

# EE ----------------------------------------------------------------------

  data(edhec)
  R <- edhec[,1:4]
  funds <- colnames(R)
  
  portf <- portfolio.spec(funds)
  portf <- add.constraint(portf, type="full_investment")
  portf <- add.constraint(portf, type="long_only")
  portf <- add.objective(portf, type="risk", name="StdDev")
  
  # Quarterly rebalancing with 5 year training period
  bt.opt1 <- optimize.portfolio.rebalancing(R, portf,
                                            optimize_method="ROI",
                                            rebalance_on="quarters",
                                            training_period=60)
  
  # Monthly rebalancing with 5 year training period and 4 year rolling window
  bt.opt2 <- optimize.portfolio.rebalancing(R, portf,
                                            optimize_method="ROI",
                                            rebalance_on="months",
                                            training_period=12,
                                            rolling_window=72)
  # }  
  extractWeights(bt.opt2)
  chart.Weights(bt.opt2, main="Weights over Time")
  