
library(quantmod)
library(forecast)
library(urca)
library(tseries)
library(rugarch)
library(fGarch)

ADBEPrices <-getSymbols.yahoo("CSCO", from="2015-01-01", to = "2019-12-31", 
                              periodicity = "daily", 
                              auto.assign=FALSE)[,6]

forcprice<- getSymbols.yahoo("CSCO", from="2020-1-1", to = "2020-1-31", 
                             periodicity = "daily", 
                             auto.assign=FALSE)[,6]
forcp <- (diff(log(forcprice$CSCO.Adjusted)))

range(na.omit(forcp))

na.omit(ADBEPrices)
plot(ADBEPrices)


library(zoo)
# create a zoo object for plotting
ADBEPrices<- zoo(ADBEPrices)
#AAPL<-getSymbols('AAPL',src='yahoo')
CSCO<-ts(diff(log(ADBEPrices$CSCO.Adjusted)))
CSCO<-(diff(log(ADBEPrices$CSCO.Adjusted)))


length(CSCO)
#Stationarity
summary(ur.kpss(CSCO, type = 'mu', lags = 'long'))

summary(ur.df(CSCO,type='trend',lags=20,selectlags='BIC'))

pacf(ts(CSCO))

plot(CSCO, col='black'); title("Returns of Cisco Systems");xlab("Time")

library(tseries)
g10<-garch(CSCO, order=c(1,0), trace=TRUE)
g11<-garch(CSCO, order=c(1,1), trace=FALSE)
g20<-garch(CSCO, order=c(2,0), trace=FALSE)
g21<-garch(CSCO, order=c(2,1), trace=FALSE)
g22<-garch(CSCO, order=c(2,2), trace=FALSE)
g02<-garch(CSCO, order=c(0,2), trace=FALSE)
g12<-garch(CSCO, order=c(1,2), trace=FALSE)

AIC(g10, g11, g20,g21,g22,g02,g12)

plot(g11)
plot(predict(g11))

summary(g11)

 predict(g11, CSCO )

 
 
library(fGarch)

g1 <- garchFit(~garch(1,1), data = CSCO, trace= FALSE)
summary(g1)

plot(g1)
 
library(rugarch)
ug1 <- ugarchspec()
ug10 <- ugarchspec(mean.model = list(armaOrder=c(1,0)))
ug11 <- ugarchspec(mean.model = list(armaOrder=c(1,1)))
ug01 <- ugarchspec(mean.model = list(armaOrder=c(0,1)))
ug20 <- ugarchspec(mean.model = list(armaOrder=c(2,0)))
ug21 <- ugarchspec(mean.model = list(armaOrder=c(2,1)))
ug22 <- ugarchspec(mean.model = list(armaOrder=c(2,2)))
ug02 <- ugarchspec(mean.model = list(armaOrder=c(0,2)))



ug1
ug10
ug11
ug01
ug20
ug21
ug22
ug02
 
      
m1 <- ugarchfit(data = CSCO, spec = ug1)
m2 <- ugarchfit(data = CSCO, spec = ug10) 
m3 <- ugarchfit(data = CSCO, spec = ug11) 
m4 <- ugarchfit(data = CSCO, spec = ug01) 
m5 <- ugarchfit(data = CSCO, spec = ug20) 
m6 <- ugarchfit(data = CSCO, spec = ug21) 
m7 <- ugarchfit(data = CSCO, spec = ug22) 
m8 <- ugarchfit(data = CSCO, spec = ug02) 

BIC(m1,m2,m3,m4,m5,m6,m7,m8)


plot(m1) 
 

#ug10 <- ugarchspec(variance.model=list(model="iGARCH", garchOrder=c(1,0)), 
#                   mean.model=list(armaOrder=c(0,0), include.mean=TRUE),  
#                  distribution.model="nig", fixed.pars=list(omega=0))

ug11 <- ugarchspec(variance.model=list(model="iGARCH", garchOrder=c(1,1)), 
                   mean.model=list(armaOrder=c(1,1), include.mean=TRUE),  
                   distribution.model="nig", fixed.pars=list(omega=0))

ug01 <- ugarchspec(variance.model=list(model="iGARCH", garchOrder=c(0,1)), 
                   mean.model=list(armaOrder=c(1,1), include.mean=TRUE),  
                   distribution.model="nig", fixed.pars=list(omega=0))

#ug20 <- ugarchspec(variance.model=list(model="iGARCH", garchOrder=c(2,0)), 
#                   mean.model=list(armaOrder=c(1,1), include.mean=TRUE),  
#                   distribution.model="nig", fixed.pars=list(omega=0))

ug21 <- ugarchspec(variance.model=list(model="iGARCH", garchOrder=c(2,1)), 
                   mean.model=list(armaOrder=c(1,1), include.mean=TRUE),  
                   distribution.model="nig", fixed.pars=list(omega=0))

ug22 <- ugarchspec(variance.model=list(model="iGARCH", garchOrder=c(2,2)), 
                   mean.model=list(armaOrder=c(1,1), include.mean=TRUE),  
                   distribution.model="nig", fixed.pars=list(omega=0))

ug02 <- ugarchspec(variance.model=list(model="iGARCH", garchOrder=c(0,2)), 
                      mean.model=list(armaOrder=c(1,1), include.mean=TRUE),  
                      distribution.model="nig", fixed.pars=list(omega=0))


m3 <- ugarchfit(data = CSCO, spec = ug11) 
m4 <- ugarchfit(data = CSCO, spec = ug01) 
m6 <- ugarchfit(data = CSCO, spec = ug21) 
m7 <- ugarchfit(data = CSCO, spec = ug22) 
m8 <- ugarchfit(data = CSCO, spec = ug02) 

AIC(m3,m4,m6,m7,m8)


ug10 = ugarchspec(variance.model = list(model = "fGARCH", garchOrder = c(1, 0), submodel = "GARCH", 
                                external.regressors = NULL, variance.targeting = TRUE), 
                   mean.model = list(armaOrder = c(0, 0), external.regressors = NULL), 
                   distribution.model = "nig", start.pars = list(), fixed.pars = list())

ug11 = ugarchspec(variance.model = list(model = "fGARCH", garchOrder = c(1, 1), submodel = "GARCH", 
                                         external.regressors = NULL, variance.targeting = TRUE), 
                   mean.model = list(armaOrder = c(0, 0), external.regressors = NULL), 
                   distribution.model = "nig", start.pars = list(), fixed.pars = list())

ug01 = ugarchspec(variance.model = list(model = "fGARCH", garchOrder = c(0, 1), submodel = "GARCH", 
                                         external.regressors = NULL, variance.targeting = FALSE), 
                   mean.model = list(armaOrder = c(0, 0), external.regressors = NULL), 
                   distribution.model = "nig", start.pars = list(), fixed.pars = list())

ug20 = ugarchspec(variance.model = list(model = "fGARCH", garchOrder = c(2, 0), submodel = "GARCH", 
                                         external.regressors = NULL, variance.targeting = TRUE), 
                   mean.model = list(armaOrder = c(0, 0), external.regressors = NULL), 
                   distribution.model = "nig", start.pars = list(), fixed.pars = list())

ug21 = ugarchspec(variance.model = list(model = "fGARCH", garchOrder = c(2, 1), submodel = "GARCH", 
                                         external.regressors = NULL, variance.targeting = TRUE), 
                   mean.model = list(armaOrder = c(0, 0), external.regressors = NULL), 
                   distribution.model = "nig", start.pars = list(), fixed.pars = list())

ug22 = ugarchspec(variance.model = list(model = "fGARCH", garchOrder = c(2, 2), submodel = "GARCH", 
                                         external.regressors = NULL, variance.targeting = TRUE), 
                   mean.model = list(armaOrder = c(0, 0), external.regressors = NULL), 
                   distribution.model = "nig", start.pars = list(), fixed.pars = list())

ug02 = ugarchspec(variance.model = list(model = "fGARCH", garchOrder = c(0, 2), submodel = "GARCH", 
                                         external.regressors = NULL, variance.targeting = FALSE), 
                   mean.model = list(armaOrder = c(0, 0), external.regressors = NULL), 
                   distribution.model = "nig", start.pars = list(), fixed.pars = list())

spec33 = ugarchspec(variance.model = list(model = "fGARCH", garchOrder = c(3, 3), submodel = "GARCH", 
                                        external.regressors = NULL, variance.targeting = TRUE), 
                  mean.model = list(armaOrder = c(0, 0), external.regressors = NULL), 
                  distribution.model = "nig", start.pars = list(), fixed.pars = list())


#m1 <- ugarchfit(data = CSCO, spec = ug1)
#m2 <- ugarchfit(data = CSCO, spec = ug10) 
m3 <- ugarchfit(data = CSCO, spec = ug11) 
m4 <- ugarchfit(data = CSCO, spec = ug01) 
#m5 <- ugarchfit(data = CSCO, spec = ug20) 
m6 <- ugarchfit(data = CSCO, spec = ug21, solver = 'hybrid') 
m7 <- ugarchfit(data = CSCO, spec = ug22) 
m8 <- ugarchfit(data = CSCO, spec = ug02) 
    

m10 <- ugarchfit(data = CSCO, spec = spec33)
        
        
infocriteria(m3)[1]

n<-1
o <- 2
model_ <- c("GARCH(1,1)","GARCH(2,1)", "GARCH(2,2)", "GARCH(0,2)")
AIC_ <- c(infocriteria(m3)[n],infocriteria(m6)[n],infocriteria(m7)[n],infocriteria(m8)[n])
BIC_<- c(infocriteria(m3)[o],infocriteria(m6)[o],infocriteria(m7)[o],infocriteria(m8)[o])
bb<- data.frame(model_,AIC_,BIC_)
bb
write.csv(bb,"C:\\Users\\cex\\OneDrive\\Documents\\R\\QuantStrat_Dev\\dump.csv")


infocriteria(m10)

plot(m6, which="all")
plot(m6)

infocriteria(m6)[1]
infocriteria(m66)[1]



ug66 = ugarchspec(variance.model = list(model = "fGARCH", garchOrder = c(2, 1), submodel = "GARCH", 
                                        external.regressors = NULL, variance.targeting = TRUE), 
                  mean.model = list(armaOrder = c(0, 0), external.regressors = NULL), 
                  distribution.model = "norm", start.pars = list(), fixed.pars = list())

m66 <- ugarchfit(data = CSCO, spec = ug66) 
plot(m66, which="all")
plot(m66)



spec<- getspec(m6)
setfixed(spec) <- as.list(coef(m6))
forc <- ugarchforecast(spec, n.ahead = 1, n.roll = 1256, data = CSCO[1:1257, ,drop=FALSE], out.sample = 1256)
sigma(forecast);
fitted(forecast)

forc = ugarchforecast(m6 ,n.ahead=50)
forc 
head(sigma(forc))
head(fitted(forc))
plot(forc)

fit<-xts(sigma(m6))
pp<-700
pq<-800
ccol <- par(mfrow=c(1,2))
plot(fit[pp:pq], type="l")
plot(xts(abs(CSCO[pp:pq])), type="l")
par(ccol)


data.frame(forcp[1:10]*100 ,forc@forecast$seriesFor*100)


exp(forc@forecast$sigmaFor[1:2])
volpred<- exp(forc@forecast$sigmaFor[1:2])
 
 
 
bb <- g11$fitted.values
bb
bbc<- as.data.frame(bb)
bb2<-as.numeric(bbc$sigt[3:1256])


ass<- as.numeric(CSCO)[3:1256]
hole<- as.list(ass)


data.frame(bb2, ass,rr1,cumsum(rr1))


app <- ifelse(bb2/ass>1,1,-1)
#rr <- ifelse(bb2/ass>0, abs(bb2),-1*abs(bb2))

rr<- ifelse(ass >0,ass,0)
rr1 <- ifelse(bb2/rr>0, abs(bb2),-1*abs(bb2))
plot(cumsum(rr1))
plot(cumsum(ass))

100*sum(app == 1)/length(app)
plot(ass,bb2)





