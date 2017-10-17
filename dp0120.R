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


#valuation at yearstart, perf in year
x4 <- ptc1d[,.(rc,as.numeric(substr(da,1,4)),pret)][,.(sum(pret,na.rm=T)),'rc,V2'][,.(rc,year=as.numeric(V2),totalret=V1)]
x5 <- ppm2td[format(date,'%m')==12][,.(yearend=as.numeric(format(date,'%Y')),rcode,ppm2t)][,openyear:=yearend-1]
x5 <- setkey(ppm2td[,year:=format(date,'%Y')],year,rcode)
x6 <- x5[getkey(x5),.(ppm2t),mult='last',.EACHI][,openforyear:=as.numeric(year)+1]
dd0120 <<- x6[x4,on=c(openforyear='year',rcode='rc')]
putt(dd0120)
dd0120a <<- distanced[rc=='W--1U-']
putt(dd0120a)
#ggplot(dd0120[openforyear==2012,],aes(ppm2t,totalret))+geom_point()


