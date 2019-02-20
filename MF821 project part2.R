data1<-read.csv("/Users/xinyilin/Downloads/MF821/1122.csv",header=T)
data2<-read.csv("/Users/xinyilin/Downloads/MF821/1123.csv",header=T)
data3<-read.csv("/Users/xinyilin/Downloads/MF821/1126.csv",header=T)
data4<-read.csv("/Users/xinyilin/Downloads/MF821/1127.csv",header=T)
data5<-read.csv("/Users/xinyilin/Downloads/MF821/1128.csv",header=T)
data6<-read.csv("/Users/xinyilin/Downloads/MF821/1129.csv",header=T)
data7<-read.csv("/Users/xinyilin/Downloads/MF821/1130.csv",header=T)
data8<-read.csv("/Users/xinyilin/Downloads/MF821/1203.csv",header=T)

"slp1122"
slp<-data8["slp1203"][,1]
acf(slp,lag.max = 100,
    type = c("correlation", "covariance", "partial"),
    plot = TRUE, na.action = na.fail, demean = TRUE)
sbp<-data4["sbp1127"][,1]
acf(sbp,lag.max = 100,
    type = c("correlation", "covariance", "partial"),
    plot = TRUE, na.action = na.fail, demean = TRUE)
flp<-data4["flp1127"][,1]
acf(flp,lag.max = 100,
    type = c("correlation", "covariance", "partial"),
    plot = TRUE, na.action = na.fail, demean = TRUE)
fbp<-data3["fbp1126"][,1]
acf(fbp,lag.max = 100,
    type = c("correlation", "covariance", "partial"),
    plot = TRUE, na.action = na.fail, demean = TRUE)

clist<-c("1122","1123","1126","1127","1128","1129","1130","1203")
library(zoo)
library(lmtest)
for(i in clist){
  data<-read.csv(paste("/Users/xinyilin/Downloads/MF821/",i,".csv",sep=""))
  x<-data[,3]
  y<-data[,6]
  granger_xy<-data.frame(Df = 0, F=0)
  granger_yx<-data.frame(Df = 0, F=0)
  for(j in 1:25)
  {
    #x is spot trade, y is forward quote
    a<-grangertest(x,y,order = j)[2,2:3]#take the values of Df and F
    b<-grangertest(y,x,order = j)[2,2:3]
    granger_xy<-rbind(granger_xy,a)
    granger_yx<-rbind(granger_yx,b)
  }
  write.csv(granger_xy,paste(i,"spot_quote_forward_quote.csv",sep = ""))
  
  write.csv(granger_yx,paste(i,"forward_quote_spot_quote.csv",sep = ""))
}
#impulse response function
library(MTS)