library(readxl)
library(car)
data <- read_excel("D:/data tesis/2017-mod5090.xlsx")

#boxplot
y<-data$Y
boxplot(y, ylab="Kadar Glukosa Darah", main="Boxplot Kadar Glukosa Darah Invasif")

#multicollinearity
mkt<-lm(Y~.,data=data)
View(vif(mkt))


#centering and scaling
Y<-data$Y
Y<-(Y-mean(Y))/sqrt(sum((Y-mean(Y))^2))
X1<-data$X1
X1<-(X1-mean(X1))/sqrt(sum((X1-mean(X1))^2))
X2<-data$X2
X2<-( X2-mean(X2))/sqrt(sum((X2-mean(X2))^2))
X3<-data$X3
X3<-( X3-mean(X3))/sqrt(sum((X3-mean(X3))^2))
X4<-data$X4
X4<-( X4-mean(X4))/ sqrt(sum((X4-mean(X4))^2))
X5<-data$X5
X5<-( X5-mean(X5))/ sqrt(sum((X5-mean(X5))^2))
X6<-data$X6
X6<-( X6-mean(X6))/ sqrt(sum((X6-mean(X6))^2))
X7<-data$X7
X7<-( X7-mean(X7))/ sqrt(sum((X7-mean(X7))^2))
X8<-data$X8
X8<-( X8-mean(X8))/ sqrt(sum((X8-mean(X8))^2))
X9<-data$X9
X9<-( X9-mean(X9))/ sqrt(sum((X9-mean(X9))^2))
X10<-data$X10
X10<-(X10-mean(X10))/ sqrt(sum((X10-mean(X10))^2))
X11<- data$X11
X11<-(X11-mean(X11))/ sqrt(sum((X11-mean(X11))^2))
X12<- data$X12
X12<-(X12-mean(X12))/ sqrt(sum((X12-mean(X12))^2))
X13<-data$X13
X13<-(X13-mean(X13))/ sqrt(sum((X13-mean(X13))^2))
X14<-data$X14
X14<-(X14-mean(X14))/ sqrt(sum((X14-mean(X14))^2))
X15<-data$X15
X15<-( X15-mean(X15))/ sqrt(sum((X15-mean(X15))^2))
X16<-data$X16
X16<-(X16-mean(X16))/ sqrt(sum((X16-mean(X16))^2))
X17<-data$X17
X17<-(X17-mean(X17))/ sqrt(sum((X17-mean(X17))^2))
X18<-data$X18
X18<-( X18-mean(X18))/ sqrt(sum((X18-mean(X18))^2))
X19<-data$X19
X19<-( X19-mean(X19))/ sqrt(sum((X19-mean(X19))^2))
X20<-data$X20
X20<-( X20-mean(X20))/ sqrt(sum((X20-mean(X20))^2))
X21<-data$X21
X21<-( X21-mean(X21))/ sqrt(sum((X21-mean(X21))^2))
X22<-data$X22
X22<-( X22-mean(X22))/ sqrt(sum((X22-mean(X22))^2))
X23<-data$X23
X23<-( X23-mean(X23))/ sqrt(sum((X23-mean(X23))^2))
X24<-data$X24
X24<-( X24-mean(X24))/ sqrt(sum((X24-mean(X24))^2))
X25<-data$X25
X25<-( X25-mean(X25))/ sqrt(sum((X25-mean(X25))^2))
data.scale<-data.frame(Y,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,
                       X23,X24,X25)

#training and testing data
library(caTools)
testing = sample.split(data.scale$Y, SplitRatio = 0.8)
dtrain=subset(data.scale,testing==TRUE)
dtest=subset(data.scale,testing==FALSE)


#robustMM
library(robustbase)
robustmm<-lmrob(Y~.-1,data=dtrain, method = "MM",maxit.scale=500)


#data matrix 
y<-matrix(dtrain$Y)
x1<-matrix(dtrain$X1)
x2<-matrix(dtrain$X2)
x3<-matrix(dtrain$X3)
x4<-matrix(dtrain$X4)
x5<-matrix(dtrain$X5)
x6<-matrix(dtrain$X6)
x7<-matrix(dtrain$X7)
x8<-matrix(dtrain$X8)
x9<-matrix(dtrain$X9)
x10<-matrix(dtrain$X10)
x11<-matrix(dtrain$X11)
x12<-matrix(dtrain$X12)
x13<-matrix(dtrain$X13)
x14<-matrix(dtrain$X14)
x15<-matrix(dtrain$X15)
x16<-matrix(dtrain$X16)
x17<-matrix(dtrain$X17)
x18<-matrix(dtrain$X18)
x19<-matrix(dtrain$X19)
x20<-matrix(dtrain$X20)
x21<-matrix(dtrain$X21)
x22<-matrix(dtrain$X22)
x23<-matrix(dtrain$X23)
x24<-matrix(dtrain$X24)
x25<-matrix(dtrain$X25)
x<-cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23,x24,x25)


#compute value of K 
n<-96
p<-25
betamm<-robustmm$coefficients
betamm<-matrix(betamm)
a<-x%*%betamm
b<-t(y-a)
c<-y-a
scale<-(b%*%c)/(n-p-1)
scale
scale<-as.numeric(scale)
scale
d<-t(betamm)%*%betamm
k<-(p*scale)/d
k<-as.numeric(k)
k

#ridge robust
I<-diag(25)
e<-t(x)%*%x
f<-I*k
g<-solve(e+f)
h<-t(x)%*%a
beta.rrmm<-g%*%h

#==========================goodness of fit for model========================================#
resid<-sum((y-x%*%beta.rrmm)^2)
rmseadj<-sqrt(resid/(n-p-1))
kebaikanmodeldtrain<-cbind(resid,rmseadj)


#===================data testing========================#
yt<-matrix(dtest$Y)
x1<-matrix(dtest$X1)
x2<-matrix(dtest$X2)
x3<-matrix(dtest$X3)
x4<-matrix(dtest$X4)
x5<-matrix(dtest$X5)
x6<-matrix(dtest$X6)
x7<-matrix(dtest$X7)
x8<-matrix(dtest$X8)
x9<-matrix(dtest$X9)
x10<-matrix(dtest$X10)
x11<-matrix(dtest$X11)
x12<-matrix(dtest$X12)
x13<-matrix(dtest$X13)
x14<-matrix(dtest$X14)
x15<-matrix(dtest$X15)
x16<-matrix(dtest$X16)
x17<-matrix(dtest$X17)
x18<-matrix(dtest$X18)
x19<-matrix(dtest$X19)
x20<-matrix(dtest$X20)
x21<-matrix(dtest$X21)
x22<-matrix(dtest$X22)
x23<-matrix(dtest$X23)
x24<-matrix(dtest$X24)
x25<-matrix(dtest$X25)
xt<-cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23,x24,x25)


#ridge robust dtest
ymmt<-xt%*%beta.rrmm

#==========================goodness of fit for data testing========================================#
resid.rrmmt<-sum((yt-ymmt)^2)
residp<-resid.rrmmt
rmsep<-sqrt((resid.rrmmt)/22)
kebaikanmodeldtest<-cbind(residp,rmsep)


