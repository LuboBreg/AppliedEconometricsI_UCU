library('BatchGetSymbols')
library(psych)
first.date <- Sys.Date() - 180
last.date <- Sys.Date()
freq.data <- 'daily'
# set tickers
tickers <- 'TSLA'#$c('FB','MMM','PETR4.SA','abcdef')

l.out <- BatchGetSymbols(tickers = tickers, 
                         first.date = first.date,
                         last.date = last.date, 
                         freq.data = freq.data,
                         cache.folder = file.path(tempdir(), 
                                                  'BGS_Cache') ) # cache in tempdir()


print(l.out$df.control)

#Review data distribution 
hist <- qplot() + geom_histogram(aes(l.out$df.tickers$price.close), bins = 15)
print(hist)

#mean
hist<- hist + geom_vline(xintercept=mean(l.out$df.tickers$price.close), color = 'red')
#meadian
hist<- hist + geom_vline(xintercept=median(l.out$df.tickers$price.close), color = 'blue')
#geometric mean 
hist<- hist + geom_vline(xintercept=geometric.mean(l.out$df.tickers$price.close), color = 'green')
#rms
getGeomean <- function(x){sqrt(mean(x^2))}
hist<- hist + geom_vline(xintercept=getGeomean(l.out$df.tickers$price.close), color = 'orange')
#mode
getMode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
hist<- hist + geom_vline(xintercept=getMode(l.out$df.tickers$price.close), color = 'violet')
print(hist)

#Dynamics over time 
p <- ggplot(l.out$df.tickers, aes(x = ref.date, y = price.close))
p <- p + geom_line()
p <- p + facet_wrap(~ticker, scales = 'free_y') 
print(p)

#mean
p<- p + geom_hline(yintercept=mean(l.out$df.tickers$price.close), color = 'red')
#meadian
p<- p + geom_hline(yintercept=median(l.out$df.tickers$price.close), color = 'blue')
#geometric mean 
p<- p + geom_hline(yintercept=geometric.mean(l.out$df.tickers$price.close), color = 'green')
#rms
getGeomean <- function(x){sqrt(mean(x^2))}
p<- p + geom_hline(yintercept=getGeomean(l.out$df.tickers$price.close), color = 'orange')
#mode
getMode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
p<- p + geom_hline(yintercept=getMode(l.out$df.tickers$price.close), color = 'violet')
print(p)

summary(l.out$df.tickers$price.close)

#Other measures 
hist <- qplot() + geom_histogram(aes(l.out$df.tickers$price.close), bins = 15, color = 'orange')
print(hist)

#Quantile 
hist <- hist  + 
  geom_vline(xintercept = quantile(l.out$df.tickers$price.close, 0.99), color = 'black') +
  geom_vline(xintercept = quantile(l.out$df.tickers$price.close, 0.01), color = 'black')
print(hist)

#Quartiles
#1st Quartile 
hist <- hist  + geom_vline(xintercept = quantile(l.out$df.tickers$price.close, 0.25), color = 'red')
#2st Quartile 
hist <- hist  + geom_vline(xintercept = quantile(l.out$df.tickers$price.close, 0.5), color = 'red')
#3st Quartile 
hist <- hist  + geom_vline(xintercept = quantile(l.out$df.tickers$price.close, 0.75), color = 'red')
print(hist)

#Boxplot 
boxplot <- qplot() + geom_boxplot(aes(y=l.out$df.tickers$price.close))
print(boxplot)

boxplot <- qplot() + geom_boxplot(aes(y=l.out$df.tickers$price.close, group = months.Date(l.out$df.tickers$ref.date)))
print(boxplot)

#Variation
library('BatchGetSymbols')
library(psych)
first.date <- Sys.Date() - 180
last.date <- Sys.Date()
freq.data <- 'daily'
# set tickers
tickers <- c('TSLA','FB')#$c('FB','MMM','PETR4.SA','abcdef')

l.out <- BatchGetSymbols(tickers = tickers, 
                         first.date = first.date,
                         last.date = last.date, 
                         freq.data = freq.data,
                         cache.folder = file.path(tempdir(), 
                                                  'BGS_Cache') ) # cache in tempdir()


print(l.out$df.control)

library(ggplot2)

#Dynamics over time 
p <- ggplot(l.out$df.tickers, aes(x = ref.date, y = price.close))
p <- p + geom_line()
p <- p + facet_wrap(~ticker, scales = 'free_y') 
print(p)


#Density  
d <- ggplot(l.out$df.tickers, aes(x = price.close))
d <- d + geom_histogram(bins= 100)
d <- d + facet_wrap(~ticker) 
print(d)

FBdf <- l.out$df.tickers[which(l.out$df.tickers$ticker == 'FB'),]
TSLAdf <- l.out$df.tickers[which(l.out$df.tickers$ticker == 'TSLA'),]

#Standart Deviation
SD(FBdf$price.close)
SD(TSLAdf$price.close)

#CV
SD(FBdf$price.close)/mean(FBdf$price.close)
SD(TSLAdf$price.close)/mean(TSLAdf$price.close)


#Empirical rule  68%
qplot(FBdf$price.close, geom = 'density', fill = 'FB') +
  geom_vline(xintercept =  (SD(FBdf$price.close) + mean(FBdf$price.close))) +
  geom_vline(xintercept =  (-SD(FBdf$price.close) + mean(FBdf$price.close)))             

#Empirical rule 95%
qplot(FBdf$price.close, geom = 'density', fill = 'FB') +
  geom_vline(xintercept =  (2*SD(FBdf$price.close) + mean(FBdf$price.close))) +
  geom_vline(xintercept =  (-2*SD(FBdf$price.close) + mean(FBdf$price.close)))               

#Correlation Covatiation 
qplot(TSLAdf$price.close,FBdf$price.close)
#Covariation
cov(TSLAdf$price.close,FBdf$price.close)

#Correlation
cor(TSLAdf$price.close,FBdf$price.close)




