library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)

stock_tickers <-  c("CSCO", "PSX", "LRCX", "TROW", "BRKS")
weights <- ww

portfolioPrices <- NULL
for(ticker in stock_tickers) {
  portfolioPrices <- cbind(portfolioPrices,
                           getSymbols.yahoo(ticker, from="2015-01-01", to = "2019-12-31", periodicity = 'daily', auto.assign=FALSE)[,6])
}

portfolioReturns <- na.omit(ROC(portfolioPrices))

PorVaR.Gaus<-VaR(portfolioReturns, p=0.95, weights=weights,portfolio_method = "component",method="gaussian")
bb <- PorVaR.Gaus$VaR
bb *1000000
write.csv(bb,"C:\\Users\\cex\\OneDrive\\Documents\\R\\QuantStrat_Dev\\dump.csv")
