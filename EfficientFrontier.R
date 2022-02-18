library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)

stock_tickers <- c("CSCO", "PSX", "LRCX", "TROW", "BRKS")

portfolioPrices <- NULL
for(ticker in stock_tickers) {
  portfolioPrices <- cbind(portfolioPrices,
                           getSymbols.yahoo(ticker, from="2015-01-01", to = "2019-12-31", periodicity = 'daily', auto.assign=FALSE)[,6])
}

portfolioReturns <- na.omit(ROC(portfolioPrices))

portf <- portfolio.spec(colnames(portfolioReturns))

portf <- add.constraint(portf, type="weight_sum", min_sum=1, max_sum=1)
portf <- add.constraint(portf, type="box", min=.10, max=.25)
portf <- add.objective(portf, type="return", name="mean")
portf <- add.objective(portf, type="risk", name="StdDev")

library(ROI)
library(ROI.plugin.glpk)
library(ROI.plugin.quadprog)

optPort <- optimize.portfolio(portfolioReturns, portf, optimize_method = "ROI", trace=TRUE, maxSR=TRUE)

weights(optPort)

ww<- as.numeric(optPort$weights)

chart.Weights(optPort) 
ef <- extractEfficientFrontier(optPort, match.col = "StdDev", n.portfolios = 10,
                         risk_aversion = NULL)



chart.EfficientFrontier(ef,
                        match.col = "StdDev", n.portfolios = 25, xlim = NULL, ylim = NULL,
                        cex.axis = 0.8, element.color = "darkgray", main = "Efficient Frontier",
                        RAR.text = "SR", rf = 0, tangent.line = TRUE, cex.legend = 0.8,
                        chart.assets = TRUE, labels.assets = TRUE, pch.assets = 21,
                        cex.assets = 0.8)


front<- as.data.frame.array(ef$frontier)
sharpe_r <- (1000*front$mean- 0.015)/(100*front$StdDev)

bb<-data.frame(1000*front$mean, 100*front$StdDev, sharpe_r)


#plot( 100*front$StdDev,0.015+(1000*front$StdDev*0.0551), type="l")
write.csv(bb,"C:\\Users\\cex\\OneDrive\\Documents\\R\\QuantStrat_Dev\\dump.csv")

vol <- seq(1, 1.4, by= 0.01)
rp <- 0.015 + 0.8886*(vol)

plot(vol, rp, type="l")


