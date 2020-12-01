library(fOptions)
library(readxl)
TSLA_data <- read_excel("C:/706 computational Finance/A5/TSLA data.xlsx")

df = TSLA_data[(3:nrow(TSLA_data)),]
#locate the current price
p1=as.numeric(df[2,1])/(1+as.numeric(df[2,2]))
#change the colnames and slice the call part
colnames(df)=df[1,]
df=df[2:105,]

df[df$Strike==250,]
#locate the most close to current value one which has the smallest abs moneyness
df[df$Strike==460,]

# set parameters 
# check IV column on 460
implied_vol = 74.24/100;p1=as.numeric(df[2,1])/(1+as.numeric(df[2,2]))
X=460 ; b=0 ; T = 65/365 ; price = 58.55

i_premium_record= matrix(0,nrow = 200,ncol = 2)
# calculate how premium vary when i ranges from 0% to 2%
for (i in 1:200){

  premium=GBSOption(TypeFlag = "c",S=p1,X=460,Time = T,r=i/10000,b=i/10000,sigma = implied_vol)
  
  i_premium_record[i,1]=i
  i_premium_record[i,2]=premium@price

}

k=79/10000
#79 base point yield the 58.5, the same as last price
GBSOption(TypeFlag = "c",S=461.7,X=460,Time =65/365, r=k ,b=k,sigma = 0.7424)
#
GBSCharacteristics(TypeFlag = "c",S=461.7,X=460,Time = 65/360,r = 0.008,b=0,sigma = 0.7424)


# in the up trend case
S1 = 461.7 *1.06;
T1=(65-10)/365;
r1=0.0008; iv1 = implied_vol*1.1

GBSCharacteristics(TypeFlag = "c",S=S1,X=460,Time = T1 ,r = 0.008,b=0,sigma = iv1)

# in the down trend case
S2 = 461.7*0.94
GBSCharacteristics(TypeFlag = "c",S=S2,X=460,Time = T1 ,r = 0.008,b=0,sigma = iv1)




# PART 2 ------------------------------------------------------------------


df2=read.csv("C:/706 computational Finance/A5/AMZN option data2.csv")

df2=df2[5:nrow(df2),c(1,3,4,5)]

# set parameters
# use given information 15-Nov-19 (31d); CSize 100; R 1.89; IFwd 1771.83
S0=1767;T1=31/365;r1=1.89/100
df2=cbind(df2, NewColumn=0)
colnames(df2)=c("strick","Bid","Ask","last","implied volatility")

df2=as.data.frame(df2)
as.numeric(as.vector(df2$strick[i]))

for (i in 1:nrow(df2)){
  S_temp = as.numeric(as.vector(df2$strick[i]))
  P_temp = as.numeric(as.vector(df2$last[i]))

  iv=GBSVolatility(P_temp,TypeFlag = "c",S=S0,X=S_temp,Time = T1,r=r1,b = 0)
  df2$`implied volatility`[i]=iv
  
}


GBSVolatility(359.5,TypeFlag = "c",S=S0,X=1410,Time = T1,r=r1,b = 0)



library(ggplot2)
ggplot(data = df2,aes(x=strick,y=`implied volatility`))+geom_point()+theme(axis.text.x = element_text(face=, color="#993333", 
                                   size=8, angle=60),
        axis.text.y = element_text(face="bold", color="#993333", 
                                   size=14, angle=90))


# draw the graph use the last value and implied volatility  
ggplot(data = df2,aes(x=strick,y=`implied volatility`))+geom_point()+scale_x_discrete(breaks=c("1500","1600","1700","1800","1900","2000"))


# draw the graph with the midpoint of bid and ask       


for (i in 1:nrow(df2)){
  S_temp = as.numeric(as.vector(df2$strick[i]))
  P_temp = (as.numeric(as.vector(df2$Bid[i]))+as.numeric(as.vector(df2$Ask[i])))/2
  
  iv=GBSVolatility(P_temp,TypeFlag = "c",S=S0,X=S_temp,Time = T1,r=r1,b = 0)
  df2$`implied volatility`[i]=iv
  
}


GBSVolatility(359.5,TypeFlag = "c",S=S0,X=1410,Time = T1,r=r1,b = 0)

ggplot(data = df2,aes(x=strick,y=`implied volatility`))+
  geom_point()+scale_x_discrete(breaks=c("1500","1600","1700","1800","1900","2000"))+
    ggtitle("use the midpoint of bid and ask as price sent to GBSvolatility")
                                                                                      

