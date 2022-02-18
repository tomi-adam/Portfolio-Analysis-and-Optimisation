library(dplyr)
library(quantmod)
library(PerformanceAnalytics)
library(imputeTS)
library(PortfolioAnalytics)


tickers <- c("CSCO", "PSX", "LRCX", "TROW", "BRKS")
#tickers <- c("^BCOM", "JJM")
#weights <- c(.20, .20, .20, .20, .20)
#weights <- c(0.1787097  ,0.1787097  , .05, .30, .30)
weights<-ww

#Get Prices (can be monthly or weekly)
portfolioPrices <- NULL
for (Ticker in tickers)
  portfolioPrices <- cbind(portfolioPrices,
                           getSymbols.yahoo(Ticker, from="2015-01-01", to = "2019-12-31", periodicity = "daily", auto.assign=FALSE)[,6])

benchmarkPrices <- getSymbols.yahoo("SPY",from="2015-01-01", to = "2019-12-31", periodicity = "daily", auto.assign=FALSE)[,6]
colSums(is.na(benchmarkPrices))
benchmarkReturns <- na.omit(ROC(benchmarkPrices, type="discrete"))

Return.cumulative(dailyReturns)
Return.cumulative(portfolioReturn)




#Rename Columns
colnames(portfolioPrices) <- tickers

#Get sum of NA per column
colSums(is.na(portfolioPrices))

#Plo
plot(portfolioPrices, legend = tickers)

#charts.PerformanceSummary(dailyReturns$BRKS)
charts.PerformanceSummary(benchmarkReturns)

#Calculate Returns For DF
dailyReturns <- na.omit(ROC(portfolioPrices, type="discrete"))




#Covariance
cov(dailyReturns)
#cor(dailyReturns$SEDG,dailyReturns$VZ)

#Correlation
bb <-cor(as.matrix(dailyReturns))
write.csv(bb,"C:\\Users\\cex\\OneDrive\\Documents\\R\\QuantStrat_Dev\\dump.csv")

#Volatility
StdDev(dailyReturns)
StdDev.annualized(portfolioReturn)*5


#Calculate Portfolio Returns
portfolioReturn <- Return.portfolio(dailyReturns, weights=weights)


#Plot Performance
chart.CumReturns(portfolioReturn)
charts.PerformanceSummary(portfolioReturn)

maxDrawdown(dailyReturns)
maxDrawdown(portfolioReturn)
#Calculate Metrics 
CAPM.beta(portfolioReturn, benchmarkReturns, .015/252)
bb <- CAPM.beta(dailyReturns, benchmarkReturns, .015/252)

StdDev.annualized(portfolioReturn)
StdDev.annualized(benchmarkReturns)

CAPM.beta.bull(portfolioReturn, benchmarkReturns, .015/252)
CAPM.beta.bear(portfolioReturn, benchmarkReturns, .015/252)

CAPM.alpha(portfolioReturn, benchmarkReturns, .015/252)
CAPM.alpha(dailyReturns, benchmarkReturns, .015/252)

CAPM.jensenAlpha(portfolioReturn, benchmarkReturns, .035/252)

SharpeRatio(portfolioReturn, Rf = .015/252, p = 0.95, FUN = "StdDev",
                      weights = NULL, annualize = TRUE)

table.AnnualizedReturns(dailyReturns, Rf=.015/252, geometric=TRUE)
table.AnnualizedReturns(portfolioReturn, Rf=.015/252, geometric=TRUE)
table.AnnualizedReturns(benchmarkReturns, Rf=.015/252, geometric=TRUE)

cov(portfolioReturn, benchmarkReturns)/var(benchmarkReturns)


#Beta
lm(dailyReturns~benchmarkReturns)
reg<-lm(portfolioReturn~benchmarkReturns)
library(gamlss)
gam<- gamlss(portfolioReturn~benchmarkReturns)

summary(gam)
summary(reg)
reg1<- as.numeric(reg$coefficients)
reg1[1]*100

histDist(portfolioReturn, family = "NO", nbins = 20)

