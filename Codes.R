
# 1.a fixed strike look_back option ---------------------------------------

S0=100;dt=1/252;T=1;Path_Length=round(T/dt,0);
r=0.05;
E=rnorm(Path_Length,0,1)
sigma=0.3
strike=95
S= matrix(NA, Path_Length+1,1);
S[1] = S0;
for (j in 1 : Path_Length) { # length of path
  
  S[j+1] = S[j] + r*S[j]* dt + sigma *S[j]*sqrt(dt)* E[j]
  
}

S=(S[100:length(S)]);
payoff = max(S)-strike;


MC_Lookback_Fix = function(Type, S0,strike, r, sigma, expire_days, n, m){
  # Type: Put or Call
  # S0: Spot price
  # strike is the predetermined strike price
  # r : interest rate
  # sigma: volatility
  # n: discrete steps frequency
  # m: number of simulations
  
  dt = 1/n# usually there are 252 trading days
  Path_Length = expire_days # how many days left before expiry it is integer
  payoff_sum = 0      # initialize the sum of payoffs that you will collect over simulated scenarios
  
  for (i in 1:m){
  
    S= matrix(NA, Path_Length,1)  
    S[1] = S0
    E=rnorm(Path_Length,0,1)# generate random noise
    
    for (j in 1 : Path_Length) { # length of path
      
      S[j+1] = S[j] + r*S[j]* dt + sigma *S[j]*sqrt(dt)* E[j]
      
    }
    
    if (Type == "c" ){ payoff = max(S)-strike }  # Call
    
    else if (Type == "p" ){ payoff = strike - min(S) }  # Put
    
    payoff_sum =  payoff_sum + payoff # Accumulate the payoffs from all simulated scenarios. 

  }
  OptionValue = (payoff_sum * exp(-r*(expire_days/n)))/m 
  return(OptionValue)
  
}


#try to calculate this kind of call option with expiry days of 125 tradings days
MC_Lookback_Fix("c",100,95,0.05,0.3,125,252,100)

# 1.b fixed stike look_vack option with time window -----------------------

#we can modify the standard fixed strike lookback option by adding a time window slice of the S path

MC_Lookback_Fix_Partial = function(Type, S0,strike, r, sigma, expire_days,waiting_days, n, m){
  # Type: Put or Call
  # S0: Spot price
  # strike is the predetermined strike price
  # r : interest rate
  # sigma: volatility
  # expire_days is how long this option lasts
  # waiting_days is how many days should be waiting before this option really starts
  # n: discrete steps frequency
  # m: number of simulations
  
  dt = 1/n# usually there are 252 trading days
  Path_Length = expire_days # how many days left before expiry it is integer
  payoff_sum = 0      # initialize the sum of payoffs that you will collect over simulated scenarios
  
  for (i in 1:m){
    
    S= matrix(NA, Path_Length+1,1)  
    S[1] = S0
    E=rnorm(Path_Length,0,1)# generate random noise
    
    for (j in 1 : Path_Length) { # length of path
      
      S[j+1] = S[j] + r*S[j]* dt + sigma *S[j]*sqrt(dt)* E[j]
      
    }
    
    
    #slice the S
    S = S[(waiting_days+1):length(S)]
    
    
    
    #after slicing there exist the possibility that max(s)-strike is negative
    #add the max(0,payoff) to cpnstrain the payoff to be positive
    if (Type == "c" ){ payoff = max(0,(max(S)-strike)) }  # Call
    
    else if (Type == "p" ){ payoff = max(0,(strike - min(S))) }  # Put
    
    payoff_sum =  payoff_sum + payoff # Accumulate the payoffs from all simulated scenarios. 
    
  }
  OptionValue = (payoff_sum * exp(-r*(expire_days/n)))/m 
  return(OptionValue)
  
}

MC_Lookback_Fix_Partial("c",100,95,0.05,0.3,125,100,252,10000)

# show how The partial-time fixed strike lookback option is cheaper than a similar standard fixed strike lookback option


b=seq(0,200,by = 10)
length(b)  
#when waiting time is 0 it is equivalent to previous function
Record=matrix(NA,length(b),1)
# when set the simulation number to be 100, we can still see some fluctuation
# setting simulation number to be more than 1000, we can see a generally monetone declining line with few exceptions
for (i in seq(0,200,by = 10)){
  Record[i]=MC_Lookback_Fix_Partial("c",100,95,0.05,0.3,252,i,252,1000)
}
plot(Record)

MC_Lookback_Fix("c",100,95,0.05,0.3,252,252,1000)




# 2 -----------------------------------------------------------------------

library(quantmod)

# get data
ticker_list = c("^VIX","TSLA")

stock_prices <- NULL

for (ticker in ticker_list){
  stock_prices <- cbind(stock_prices, getSymbols.yahoo(ticker, 
                                                       from = '2020-10-14', periodicity= 'daily', auto.assign=FALSE)[,4])
}

stock_prices$VIX.Close[1];

stock_prices$TSLA_IV = NA

df=stock_prices
# use the previous IV data as initials
#460	-0.37%	59.05	59.28	59.5	58.55	8.1	16.06%	176	974	74.24%

df[1,"TSLA_IV"] = 74.24
# 7 locates the OCT 22
for (i in 2:11){
  df[i,"TSLA_IV"] = as.numeric(df[i-1,"TSLA_IV"] )*0.95/2+as.numeric(df[i,"VIX.Close"])*2.9/2+10*rnorm(1,0,1)
  
  
}
df


# 100 contract covers 100*100 shares of stock
library(fOptions)
delta1=GBSCharacteristics(TypeFlag = "c",S=461.7,X=460,Time = 65/365,r = 0.008,b=0.008,sigma = 0.7424)$delta
# holding the call option is a kind of long delta strategy
# generally we should sell some underlying asset to make the whole portfolio delta hedged

#for the fisrt day of 14th OCT 

call_delta = 100*delta1*100
stock_position = -call_delta
# call_delta is also the number of share we need to short sell to make the whole portfolio delta neutral

df$TSLA_position=NA
df

for (i in 1:11){
  S1=as.numeric(df[i,"TSLA.Close"])
  sigma1 = as.numeric(df[i,"TSLA_IV"])
  delta1=GBSCharacteristics(TypeFlag = "c",S=S1,X=460,Time = ((66-i)/365),r = 0.008,b=0.008,sigma = sigma1/100 )$delta
  
  df[i,"TSLA_position"] = -100*delta1*100
  
}
df
#generate daily position change
df$position_change=NA
df$position_change[1]=df$TSLA_position[1]

for (i in 2:11){
  df$position_change[i]=as.numeric(df$TSLA_position[i])-as.numeric(df$TSLA_position[i-1])
}
df$position_change
#this vector shows the daily position change of TSLA stock to make the whole portfolio risk neutral
#short the stock can get cash inflow and buy the stock will cause cash outflow

CF=as.vector(df$position_change*df$TSLA.Close*(-1))
CF=CF[1:11]
sum(CF)
cat("the delta hedged strategy generates the total cash inflow of ",sum(CF))

Cost_to_buyback=-df$TSLA_position[11]*-df$TSLA.Close[11]
cat("the whole portfolio still shorts the ",-df$TSLA_position[11],"shares of TSLA and they worths",-df$TSLA_position[11]*-df$TSLA.Close[11])


cat("the money makes from the short selling stock parts",as.numeric(sum(CF)+Cost_to_buyback),"theoritically it should equals the money loses on the price of call option")
cat("each call option predicted to lose",as.numeric(sum(CF)+Cost_to_buyback)/10000)
#theoritically it should equals the money loses on the price of call option