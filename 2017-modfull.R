library(readxl)
library(car)
data<- read_excel("D:/data tesis/2017-modfull.xlsx")

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
X26<-data$X26
X26<-( X26-mean(X26))/ sqrt(sum((X26-mean(X26))^2))
X27<-data$X27
X27<-( X27-mean(X27))/ sqrt(sum((X27-mean(X27))^2))
X28<-data$X28
X28<-( X28-mean(X28))/ sqrt(sum((X28-mean(X28))^2))
X29<-data$X29
X29<-( X29-mean(X29))/ sqrt(sum((X29-mean(X29))^2))
X30<-data$X30
X30<-( X30-mean(X30))/ sqrt(sum((X30-mean(X30))^2))
X31<-data$X31
X31<-( X31-mean(X31))/ sqrt(sum((X31-mean(X31))^2))
X32<-data$X32
X32<-( X32-mean(X32))/ sqrt(sum((X32-mean(X32))^2))
X33<-data$X33
X33<-( X33-mean(X33))/ sqrt(sum((X33-mean(X33))^2))
X34<-data$X34
X34<-( X34-mean(X34))/ sqrt(sum((X34-mean(X34))^2))
X35<-data$X35
X35<-( X35-mean(X35))/ sqrt(sum((X35-mean(X35))^2))
X36<-data$X36
X36<-( X36-mean(X36))/ sqrt(sum((X36-mean(X36))^2))
X37<-data$X37
X37<-( X37-mean(X37))/ sqrt(sum((X37-mean(X37))^2))
X38<-data$X38
X38<-( X38-mean(X38))/ sqrt(sum((X38-mean(X38))^2))
X39<-data$X39
X39<-( X39-mean(X39))/ sqrt(sum((X39-mean(X39))^2))
X40<-data$X40
X40<-( X40-mean(X40))/ sqrt(sum((X40-mean(X40))^2))
X41<-data$X41
X41<-( X41-mean(X41))/ sqrt(sum((X41-mean(X41))^2))
X42<-data$X42
X42<-(X42-mean(X42))/ sqrt(sum((X42-mean(X42))^2))
X43<-data$X43
X43<-(X43-mean(X43))/ sqrt(sum((X43-mean(X43))^2))
X44<-data$X44
X44<-(X44-mean(X44))/ sqrt(sum((X44-mean(X44))^2))
X45<-data$X45
X45<-(X45-mean(X45))/ sqrt(sum((X45-mean(X45))^2))
X46<-data$X46
X46<-(X46-mean(X46))/ sqrt(sum((X46-mean(X46))^2))
X47<-data$X47
X47<-(X47-mean(X47))/ sqrt(sum((X47-mean(X47))^2))
X48<-data$X48
X48<-(X48-mean(X48))/ sqrt(sum((X48-mean(X48))^2))
X49<-data$X49
X49<-(X49-mean(X49))/ sqrt(sum((X49-mean(X49))^2))
X50<-data$X50
X50<-(X50-mean(X50))/ sqrt(sum((X50-mean(X50))^2))
data.scale<-data.frame(Y,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27
                       ,X28,X29,X30,X31,X32,X33,X34,X35,X36,X37,X38,X39,X40,X41,X42,X43,X44,X45,X46,X47,X48,X49,X50)

#membagi data training dan testing
library(caTools)
testing = sample.split(data.scale$Y, SplitRatio = 0.8)
dtrain=subset(data.scale,testing==TRUE)
dtest=subset(data.scale,testing==FALSE)


#robustMM
library(robustbase)
robustmm<-lmrob(Y~.-1,data=dtrain, method = "MM",maxit.scale=500)


# data matrix 
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
x26<-matrix(dtrain$X26)
x27<-matrix(dtrain$X27)
x28<-matrix(dtrain$X28)
x29<-matrix(dtrain$X29)
x30<-matrix(dtrain$X30)
x31<-matrix(dtrain$X31)
x32<-matrix(dtrain$X32)
x33<-matrix(dtrain$X33)
x34<-matrix(dtrain$X34)
x35<-matrix(dtrain$X35)
x36<-matrix(dtrain$X36)
x37<-matrix(dtrain$X37)
x38<-matrix(dtrain$X38)
x39<-matrix(dtrain$X39)
x40<-matrix(dtrain$X40)
x41<-matrix(dtrain$X41)
x42<-matrix(dtrain$X42)
x43<-matrix(dtrain$X43)
x44<-matrix(dtrain$X44)
x45<-matrix(dtrain$X45)
x46<-matrix(dtrain$X46)
x47<-matrix(dtrain$X47)
x48<-matrix(dtrain$X48)
x49<-matrix(dtrain$X49)
x50<-matrix(dtrain$X50)
x<-cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23,x24,x25,x26,x27
         ,x28,x29,x30,x31,x32,x33,x34,x35,x36,x37,x38,x39,x40,x41,x42,x43,x44,x45,x46,x47,x48,x49,x50)


#compute value of k 
n<-96
p<-50
beta<-robustmm$coefficients
beta<-matrix(beta)
a<-x%*%beta
b<-t(y-a)
c<-y-a
scale<-(b%*%c)/(n-p-1)
scale
scale<-as.numeric(scale)
scale
d<-t(beta)%*%beta
k<-(p*scale)/d
k<-as.numeric(k)
k

#ridge robust
I<-diag(50)
e<-t(x)%*%x
f<-I*k
g<-solve(e+f)
h<-t(x)%*%a
beta.rr<-g%*%h

#==========================godness of fit========================================#
resid<-sum((y-x%*%beta.rr)^2)
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
x26<-matrix(dtest$X26)
x27<-matrix(dtest$X27)
x28<-matrix(dtest$X28)
x29<-matrix(dtest$X29)
x30<-matrix(dtest$X30)
x31<-matrix(dtest$X31)
x32<-matrix(dtest$X32)
x33<-matrix(dtest$X33)
x34<-matrix(dtest$X34)
x35<-matrix(dtest$X35)
x36<-matrix(dtest$X36)
x37<-matrix(dtest$X37)
x38<-matrix(dtest$X38)
x39<-matrix(dtest$X39)
x40<-matrix(dtest$X40)
x41<-matrix(dtest$X41)
x42<-matrix(dtest$X42)
x43<-matrix(dtest$X43)
x44<-matrix(dtest$X44)
x45<-matrix(dtest$X45)
x46<-matrix(dtest$X46)
x47<-matrix(dtest$X47)
x48<-matrix(dtest$X48)
x49<-matrix(dtest$X49)
x50<-matrix(dtest$X50)
xt<-cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23,x24,x25,x26,x27
          ,x28,x29,x30,x31,x32,x33,x34,x35,x36,x37,x38,x39,x40,x41,x42,x43,x44,x45,x46,x47,x48,x49,x50)


#ridge robust dtest
yduga<-xt%*%beta.rr

#==========================godness of fit data testing========================================#
residp<-sum((yt-yduga)^2)
rmsep<-sqrt(residp/22)
kebaikanmodeldtest<-cbind(residp,rmsep)
