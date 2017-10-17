require(aappd)
setreturn(F)
ver <- 45
app <- 'ppd'
source('../pkg/script/init.R')
aatopselect('aappd')
source('../pkg/R/step.R')
source('../pkg/R/lib.R')
#getlast <- gett
save(ver,file='ver.RData')
d0 <- '2007-12-31'
rcx <- '.'



getgd(c('dezod','ppm2td','ptc1d','distanced'))
#tp1=c('1995-12-31','1997-06-30','2009-06-30','2014-06-30')
tp1 <- c('2001-12-31','2005-12-31','2014-09-30','2017-12-31')
tp2 <- as.Date(tp1)
d0 <- as.Date('1995-03-31')


rcx <- c('W--8--','L--7--')
setkey(ptc1d,rc)
dd140a <- ptc1d[rcx][,cr:=cumsum(pret),rc][,date:=as.Date(da)]
putt(dd140a)

#---for server
i=4
tp1 <- c('2001-12-31','2005-12-31','2014-09-30','2017-12-31')
tp2 <- as.Date(tp1)
yrange <- dd140a[,range(cr,na.rm=T)]
x2 <- copy(dd140a)[date>tp2[i],cr:=NA]
x3 <- ggplot(x2,aes(date,cr,color=rc)) + 
  geom_line(aes(color=rc))+
  ylim(yrange) + 
  ylab('cumulative log return') + xlab()
ggplotly(x3)
       