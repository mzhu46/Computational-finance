library(readr)
library(fPortfolio)
library(PortfolioAnalytics)
library(PerformanceAnalytics)

set.seed(100)
data1 <- read_csv("C:/706 computational Finance/A7/data1.csv", 
                  col_types = cols(`CTA Global` = col_number(), 
                                   `Convertible Arbitrage` = col_number(), 
                                   `Distressed Securities` = col_number(), 
                                   `Emerging Markets` = col_number(), 
                                   `Equity Market Neutral` = col_number(), 
                                   `Event Driven` = col_number(), `Fixed Income Arbitrage` = col_number(), 
                                   `Funds Of Funds` = col_number(), 
                                   `Global Macro` = col_number(), `Long/Short Equity` = col_number(), 
                                   `Merger Arbitrage` = col_number(), 
                                   `Relative Value` = col_number(), 
                                   `Short Selling` = col_number(), date = col_character()))


df=as.data.frame(data1[,2:14])
rownames(df)=as.vector(as.matrix(data1[,1]))

df=df/100
df1 = df[1:132,]

fund.names <- colnames(data1[,2:14])

str(returns)

portf <- portfolio.spec(assets=fund.names) # Create a portfolio object




#df$`Convertible Arbitrage`=as.numeric(df$`Convertible Arbitrage`)
#typeof(as.numeric(df$`Convertible Arbitrage`))
#typeof(df$`Convertible Arbitrage`)

# Add some basic constraints

portf <- add.constraint(portf, type="full_investment")
portf <- add.constraint(portf, type="long_only")

portf.minStdDev <- add.objective(portf, type="risk", name="StdDev")

y_minStd = optimize.portfolio(df1, portfolio = portf.minStdDev, constraints = NULL,
                              objectives = NULL, optimize_method = c("random"), search_size = 10000, trace = FALSE)

y_minStd$weights

y_minStd$objective_measures



# for the second decade ---------------------------------------------------
#locate the data range
rownames(df[157:nrow(df),])

df2=df[157:nrow(df),]
y_minStd_2 = optimize.portfolio(df2, portfolio = portf.minStdDev, constraints = NULL,
                              objectives = NULL, optimize_method = c("random"), search_size = 10000, trace = FALSE)

y_minStd_2$weights

plot(y_minStd$weights)


# combine two data vector into a df  --------------------------------------


T1 = data.frame(rbind(y_minStd$weights,y_minStd_2$weights))
#doing the parplot compare how the strategy allocate the money in two different decade
#they are very similar, both focus on the Equity.Market.Neutral this class
T1
barplot((as.matrix(T1)), beside=TRUE,main = "Minimum Variance Portfolio Weights in two decades")

