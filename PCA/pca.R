prices <- read.csv('data/stock_prices.csv')
prices[1,]

library('lubridate')
prices <- transform(prices, Date = ymd(Date))

library('reshape')
date.stock.matrix <- cast(prices, Date ~ Stock, value = 'Close')

prices <- subset(prices, Date != ymd('2002-02-01'))
prices <- subset(prices, Stock != 'DDR')

date.stock.matrix <- cast(prices, Date ~ Stock, value = 'Close')

cor.matrix <- cor(date.stock.matrix[,2:ncol(date.stock.matrix)])
correlations <- as.numeric(cor.matrix)

library('ggplot2')
ggplot(data.frame(Correlation = correlations), aes(x = Correlation, fill = 1)) + geom_density() + theme(legend.position = 'none')

pca <- princomp(date.stock.matrix[, 2:ncol(date.stock.matrix)])

principal.component <- pca$loadings[,1]
loadings <- as.numeric(principal.component)
ggplot(data.frame(Loading = loadings), aes(x = Loading, fill = 1)) + geom_density() + theme(legend.position = 'none')

market.index <- predict(pca)[,1]

dji.prices <- read.csv('data/DJI.csv')
dji.prices <- transform(dji.prices, Date = ymd(Date))
dji.prices <- subset(dji.prices, Date > ymd('2001-12-31'))
dji.prices <- subset(dji.prices, Date != ymd('2002-02-01'))

dji <- with(dji.prices, rev(Close))
dates <- with(dji.prices, rev(Date))

comparison <- data.frame(Date = dates, MarketIndex = market.index)

ggplot(comparison, aes(x = MarketIndex, y = dji)) + geom_point() + geom_smooth(method = 'lm', se = FALSE)

alt.comparison <- melt(comparison, id.vars = 'Date')
names(alt.comparison) <- c('Date', 'Index', 'Price')

ggplot(alt.comparison, aes(x = Date, y = Price, group = Index, color = Index)) + geom_point() + geom_line()

comparison <- transform(comparison, MarketIndex = scale(MarketIndex))
comparison <- transform(comparison, dji = scale(dji))

alt.comparison <- melt(comparison, id.vars = 'Date')
names(alt.comparison) <- c('Date', 'Index', 'Price')

p <- ggplot(alt.comparison, aes(x = Date, y = Price, group = Index, color = Index)) + geom_point() + geom_line()
print(p)
