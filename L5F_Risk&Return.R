#Variation
library('BatchGetSymbols')
library(psych)
library(ggplot2)
library(zoo)
library(tidyquant)

#Functions definitions
return <- function(price){
  r <- c(0)
  for(date in 2:length(price)){
    return <- price[date]/price[date -1] -1
    r <- rbind(r, return)
  }
  r
}

#Load a data 
first.date <- Sys.Date() - 180
last.date <- Sys.Date()
freq.data <- 'daily'
# set tickers
tickers <- c('TSLA','FB','DJI')#$c('FB','MMM','PETR4.SA','abcdef')

l.out <- BatchGetSymbols(tickers = tickers, 
                         first.date = first.date,
                         last.date = last.date, 
                         freq.data = freq.data
#                       ,cache.folder = file.path(tempdir(), 
#                                                  'BGS_Cache') 
) # cache in tempdir()


#Data manipulation to isolate data from different stocks
tsla <- l.out$df.tickers[which(l.out$df.tickers$ticker == 'TSLA'),]
fb <- l.out$df.tickers[which(l.out$df.tickers$ticker == 'FB'),]
#Portfolio
pfl <- cbind(fb[,c(4,7)], tsla[,c(4,7)])
pfl$avg_price <- (pfl[,1] + pfl[,3])/2
#Market 
dji <- l.out$df.tickers[which(l.out$df.tickers$ticker == 'DJI'),]

#Investing in one one stock 
p<-ggplot() + 
  geom_line(aes(fb$ref.date, fb$price.close,  colour = 'Facebook')) +
  geom_line(aes(tsla$ref.date, tsla$price.close, colour = 'Tesla'))  + theme_bw()
print(p)   


#Risk invidivdual
risk_fb <- SD(fb$price.close)/mean(fb$price.close)
risk_fb

risk_tsla <- SD(tsla$price.close)/mean(tsla$price.close)
risk_tsla

#Portfolio 
p<-ggplot() + 
  geom_line(aes(fb$ref.date, fb$price.close, colour = 'Facebook')) +
  geom_line(aes(tsla$ref.date, tsla$price.close,colour = 'Tesla')) + 
  geom_line(aes(pfl$ref.date, pfl$avg_price, colour = 'Portfolio'))  + theme_bw()
print(p)   

#Portfolio risk 
risk_pfl<- SD(pfl$avg_price)/mean(pfl$avg_price)
risk_pfl

#Porflolio_return
paste((geometric.mean(1+pfl$return_pfl)-1)*100,"%")


#Market and Beta
p<-ggplot() + 
  geom_line(aes(dji$ref.date, (dji$price.close), colour = 'DJIA')) +
  geom_line(aes(pfl$ref.date, (pfl$avg_price ), colour = 'Portfolio')) +
  theme_bw()
print(p)   


p<-ggplot() + 
  geom_line(aes(dji$ref.date, (dji$price.close-mean(dji$price.close))/SD(dji$price.close), colour = 'DJIA')) +
  geom_line(aes(pfl$ref.date, (pfl$avg_price - mean(pfl$avg_price))/SD(pfl$avg_price), colour = 'Portfolio')) +
  ylab('Normalized prices') + theme_bw()
print(p)   

#Market return 
dji$return_dji <- as.vector(return(dji$price.close))
pfl$return_pfl <- as.vector(return(pfl$avg_price))
pm <- cbind(pfl, dji)

qplot(pm$return_pfl, pm$return_dji) + geom_smooth(method = 'lm') + theme_bw()

#Beta 
cov(pm$return_dji, pm$return_pfl)/SD(pm$return_dji)^2
  #Or linear model ?
summary(lm(return_pfl ~ return_dji, pm))


