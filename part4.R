dataset1=read.csv('/Users/romilgopani/Downloads/MF821/KUZ7_cleaned.csv')
dataset2=read.csv('/Users/romilgopani/Downloads/MF821/USDKRW_cleaned.csv')

l = dataset1[1:2000,2:4]
for(i in 0:102){
  VARselect(dataset1[1:2000+i*10,2:4],lag.max = 25,type='const')
  varstock=VAR(dataset1[1:2000+i*10,2:4],ic='AIC',p=25)
  predict1=predict(varstock,n.ahead=10,ci=0.95)$fcst
  new1=cbind(lastprice = predict1[["lastprice"]][,1],bid = predict1[["bid"]][,1],ask = predict1[["ask"]][,1])
  l = rbind(l,new1)
}
plot(l[,1], xlab = "time" , ylab = "price")
l2 = dataset2[1:2000,2:4]
for(i in 0:103){
  VARselect(dataset2[1:2000+i*10,2:4],lag.max = 25,type='const')
  varstock2=VAR(dataset1[1:2000+i*10,2:4],ic='AIC',p=25)
  predict2=predict(varstock,n.ahead=10,ci=0.95)$fcst
  new2=cbind(lastprice = predict2[["lastprice"]][,1],bid = predict1[["bid"]][,1],ask = predict1[["ask"]][,1])
  l2 = rbind(l,new2)
}
plot(l2[,1], xlab = "time" , ylab = "price")
library(Metrics)

#trading strategy
m1 <- runMean(l[,1],5) #movind average with 5 minutes of data
sd1 <- runSD(l[,1],5) #standard deviation of those 5 min
upper1 <- m1 + sd1    #upper band for the stock trading strategy
lower1 <- m1 - sd1    #lower band for the stock trading strategy
m2 <- runMean(l2[,1],5) #movind average with 5 minutes of stock data
sd2 <- runSD(l2[,1],5)   #standard deviation of those 5 min
upper2 <- m2 +  sd2    #upper band for the futures trading strategy
lower2 <- m2 -  sd2    #lower band for the future trading strategy


#x is upper band
#y is lower band
#p is the predicted price
#c is the current price
#a is the ask price
#b is the bid price

trade1 <- function(x,y,p,c,a,b)
{
  q = 0
  cash = 0
  for (i in 2000:3020)
  {
    if((p[i]>x[i]) & (p[i]<a[i]) & q!=0)
    {
      cash = cash + q*c[i]
      q = 0
      
    }
    if((p[i]<y[i]) & (p[i]>b[i]))
    {
      q = q + 1
      cash = cash - c[i]
    
    }
    
  }
  if (q!= 0)
  {
    cash = cash + q*c[3021]
  }
  return(cash)
}

trade_random <- function(a,b)
{
  q = 0
  cash = 0
  rd <- runif(1022,-1,1)
  for (i in 1:1021)
  {
    if((rd[i] < -0.7) & (q!=0))
    {
      cash = cash + q*a[i + 1999]
      q = 0
    }
    if(rd[i] > 0.7)
    {
      q = q + 1
      cash = cash - b[i + 1999]
      
    }
  }
  if (q!= 0)
  {
    cash = cash + q*dataset1[3021,2]
  }
  return(cash)
}

rmc <- function(n){
  sum = 0
  for (i in 1:n)
  {
    sum = sum + trade_random(dataset1[1:3020,3],dataset1[1:3020,4])
  }
  sum  = sum/n
  return(sum)
}

rmc2 <- function(n){
  sum = 0
  for (i in 1:n)
  {
    sum = sum + trade_random(dataset2[1:3020,3],dataset2[1:3020,4])
  }
  sum  = sum/n
  return(sum)
}


#TEST
# strategy on stock data
# trade1(upper1,lower1,l[1:3020,1],dataset1[1:3025,2],dataset1[1:3025,3],dataset1[1:3025,4])

#strategy on futures data
# trade1(upper2,lower2,l2[1:3020,1],dataset2[1:3025,2],dataset2[1:3025,3],dataset2[1:3025,4])

#random strategy on stock data for 10000 simulations
#rmc1(10000)

#random strategy on futures data for 10000 simulation
#rmc2(10000)

#Graph for stock data till 2000 data entries grouped with 1300 predicted values
#plot(l[,1], xlab = "time" , ylab = "price")

#Graph for stock data till 2000 data entries grouped with 1300 predicted values
#plot(l2[,1], xlab = "time" , ylab = "price")

