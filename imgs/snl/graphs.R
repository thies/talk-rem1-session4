
try( setwd(dirname(rstudioapi::getActiveDocumentContext()$path)))


reits <- read.csv("snl-export.csv", as.is=TRUE)

reits$debt <- as.numeric(gsub(',','',reits$SP_TOTAL_DEBT))
reits$assets <- as.numeric(gsub(',','',reits$SP_TOTAL_ASSETS))
reits$marketcap <- as.numeric(gsub(',','',reits$SP_MARKETCAP))/1000

reits <- subset(reits, assets > 0)
reits$leverage <- reits$debt/reits$assets
reits <- subset(reits, leverage < 1)

country <- table(reits$SP_COUNTRY_NAME)
country

countries <- c('USA','Canada','United Kingdom','South Africa','Singapore','France','Netherlands')


levcount <- tapply(reits$leverage, reits$SP_COUNTRY_NAME, mean)

top10 <- list()
for(country in countries){
  cou <- subset(reits, SP_COUNTRY_NAME == country & !is.na(marketcap)) 
  cou$relsize <- cou$marketcap*4/max(cou$marketcap, na.rm=TRUE)
  cou <- cou[rev(order(cou$marketcap)),]
  svg(file=paste(country, '.svg', sep=''), width=8, height=4)
  par(mar=c(4,4,0.2,0.2))
  plot(range(cou$marketcap, na.rm=TRUE),
       c(0,1),
       xlab='Market Cap (in $B)',
      ylab='Debt/total assets',
    type='n')
  points(cou$marketcap, 
       cou$leverage, 
       pch=19, 
       col='red',
       cex=cou$relsize)
  labels <- cou[1:min(10, nrow(cou)),]
  top10[[country]] <- labels
  text(labels$marketcap, labels$leverage, labels$SP_ENTITY_NAME, cex=.6)
  dev.off()
}

top10 <- do.call( rbind, top10)

top10$SP_COUNTRY_NAME[top10$SP_COUNTRY_NAME=='United Kingdom'] <- 'UK'
top10$SP_COUNTRY_NAME[top10$SP_COUNTRY_NAME=='Netherlands'] <- 'NL'
top10$SP_COUNTRY_NAME[top10$SP_COUNTRY_NAME=='South Africa'] <- 'SA'

meanlev <- tapply(top10$leverage, top10$SP_COUNTRY_NAME, mean) 
meanlev <- meanlev[order(meanlev)]
svg('meanlev.svg', width = 8, height = 4)
par(mar=c(2,4,1,1))
barplot(meanlev, main=NA, col='lightblue',
        ylim=c(0,.6))
box()
dev.off()
