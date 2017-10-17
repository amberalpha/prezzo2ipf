require(aappd)
setreturn(F)
ver <- 45
app <- 'ppd'
source('init.R')
aatopselect('aappd')
#getlast <- gett
save(ver,file='ver.RData')
d0 <- '2007-12-31'
rcx <- '.'

getgd(c('pvrcd','hvrcd','ppm2td','dezo2d'))


npc <- ppm2td[,length(unique(rcode))]
x1a <- setkey(ppm2td,date,ppm2t)[date==as.Date('2001-12-31'),][round(npc*.25)]
x1b <- setkey(ppm2td,date,ppm2t)[date==as.Date('2001-12-31'),][round(npc*.75)]
rv1 <- x1b[,ppm2t]/x1a[,ppm2t]
x2a <- setkey(ppm2td,date,ppm2t)[date==as.Date('2017-09-30'),][round(npc*.25)]
x2b <- setkey(ppm2td,date,ppm2t)[date==as.Date('2017-09-30'),][round(npc*.75)]
rv2 <- x2b[,ppm2t]/x2a[,ppm2t]
xx0150a <- data.table(x1a1=x1a[,irregpcode(rcode)],x1a2=x1a[,signif(ppm2t,3)],x1b1=x1b[,irregpcode(rcode)],x1b2=x1b[,signif(ppm2t,3)],
                     x2a1=x2a[,irregpcode(rcode)],x2a2=x2a[,signif(ppm2t,3)],x2b1=x2b[,irregpcode(rcode)],x2b2=x2b[,signif(ppm2t,3)])
putt(xx0150a)

x1 <- ppm2td[,.('qp1'=quantile(ppm2t,probs=c(.01)),'q1'=quantile(ppm2t,probs=c(.25)),'q3'=quantile(ppm2t,probs=c(.75)),'qp99'=quantile(ppm2t,probs=c(.99)),q2=median(ppm2t)),date][,.(date,qp1,q1,q3,qp99,iqr=q3/q1,ipr=qp99/qp1)]
setnames(x1,c('date','      1','   25','   75','   99','75/25','99/ 1'))
x2 <- unfactordt(melt(x1,id.var='date'))
xx0150b <- x2
putt(xx0150b)

x2 <- xx0150b
x3 <- ggplot(x2[grepl('^75/25',variable)],aes(date,value)) + 
  geom_line(aes(color=variable),size=.7) + 
  ylab('relative £/m2, quartile 4/quartile 1') + 
  theme(legend.position="none")
ggplotly(x3)
