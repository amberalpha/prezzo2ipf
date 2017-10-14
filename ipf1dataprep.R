require(aappd)
setreturn(F)
ver <- 41
app <- 'ppd'
source('script/init.R')
aatopselect('aappd')
source('../pkg/R/stepclean.R')
source('../pkg/R/lib.R')
#getlast <- gett
save(ver,file='ver.RData')
d0 <- '2007-12-31'
rcx <- '.'


#-------------------------slide 020 ppm2 dispersion
getgd(c('pvrcd','hvrcd','ppm2td','dezod'))
ppmx3 <- ppm2td[date==as.Date('2017-09-30')]

#020
x1 <- ppm2td[,.('qp1'=quantile(ppm2t,probs=c(.01)),'q1'=quantile(ppm2t,probs=c(.25)),'q3'=quantile(ppm2t,probs=c(.75)),'qp99'=quantile(ppm2t,probs=c(.99)),q2=median(ppm2t)),date][,.(date,qp1,q1,q3,qp99,iqr=q3/q1,ipr=qp99/qp1)]
setnames(x1,c('date','      1','   25','   75','   99','75/25','99/ 1'))
xx020 <- melt(x1,id.var='date')
putt(xx020)

#030
npc <- ppm2td[,length(unique(rcode))]
x1a <- setkey(ppm2td,date,ppm2t)[date==as.Date('2001-12-31'),][round(npc*.25)]
x1b <- setkey(ppm2td,date,ppm2t)[date==as.Date('2001-12-31'),][round(npc*.75)]
rv1 <- x1b[,ppm2t]/x1a[,ppm2t]
x2a <- setkey(ppm2td,date,ppm2t)[date==as.Date('2017-09-30'),][round(npc*.25)]
x2b <- setkey(ppm2td,date,ppm2t)[date==as.Date('2017-09-30'),][round(npc*.75)]
rv2 <- x2b[,ppm2t]/x2a[,ppm2t]
xx030a <- data.table(x1a1=x1a[,irregpcode(rcode)],x1a2=x1a[,signif(ppm2t,3)],x1b1=x1b[,irregpcode(rcode)],x1b2=x1b[,signif(ppm2t,3)],
                     x2a1=x2a[,irregpcode(rcode)],x2a2=x2a[,signif(ppm2t,3)],x2b1=x2b[,irregpcode(rcode)],x2b2=x2b[,signif(ppm2t,3)])
putt(xx030a)


x1 <- ppm2td[,.('qp1'=quantile(ppm2t,probs=c(.01)),'q1'=quantile(ppm2t,probs=c(.25)),'q3'=quantile(ppm2t,probs=c(.75)),'qp99'=quantile(ppm2t,probs=c(.99)),q2=median(ppm2t)),date][,.(date,qp1,q1,q3,qp99,iqr=q3/q1,ipr=qp99/qp1)]
setnames(x1,c('date','      1','   25','   75','   99','75/25','99/ 1'))
x2 <- unfactordt(melt(x1,id.var='date'))
xx030b <- x2
putt(xx030b)

#040
x1 <- dezod[,.(zo,r1y)]
x2 <- ppm2td[date==as.Date('2016-09-30'),.(rcode,ppm2t)]
xx040d <- x1[x2,on=c(zo='rcode')]
putt(xx040d)

#041
x1 <- dezod[,.(zo,r1y)]
x2 <- ppm2td[date==as.Date('2016-09-30'),.(rcode,ppm2t)]
x3 <- x1[x2,on=c(zo='rcode')][,area:=irregpcode(substr(zo,1,3))]
xx041d <- x3
putt(xx041d)

#050
if(F) {
getgd('decd')
x1 <- data.table(data.frame(lapply(lapply(decd,rollsumr,4),as.numeric)))
r <- rep(NA,4)
x <- NA
x[1] <- summary(lm(ret~re1,data=x1))$r.squared
x[2] <- summary(lm(ret~re1+re2,data=x1))$r.squared
x[3] <- summary(lm(ret~re1+re2+re3,data=x1))$r.squared
r[1] <- x[1]
r[2] <- x[2]-x[1]
r[3] <- x[3]-x[2]
r[4] <- 1-x[3]
x1 <- data.table(value=r,variable=c('factor 1','factor 2','factor 3','residual'))
xx050d <- data.frame(x1)
putt(xx050d)
}

#060 trans count
if(F) {
  getgd(c('prppd','segd'))
  x1 <- data.table(tottran=nrow(prppd),totid=prppd[,length(unique(id))],rpttran=nrow(segd),repid=segd[,length(unique(id))],tran2017=prppd[year==2017,.N],rep2017=segd[year==2017,.N])
  setnames(x1,c('transactions','unique id','repeat transactions','unique repeat id','transactions 2017','repeats 2017'))
  xx060d <- melt(x1)
  putt(xx060d)
}



#070 was piechart - dropped

#080 vintages
getgd('lmrhsd')
getgd('vind')
getgd('augd')
setkey(vind,rcode)
rcx <- vind[is.na(n),sort(unique(rcode))]
identical(rcx,augd[,sort(unique(pruneto))]) #those augmented have no total row because tricky
vindx <- setkey(vind,rcode)[!rcx][,t1:=as.numeric(y1)]
x1 <- vindx[(t1-y0)<2,.(flip=sum(n,na.rm=T)),rcode][vindx[(t1-y0)>=2,.(noflip=sum(n,na.rm=T)),rcode]][,.(rcode,flip/noflip)]
setkey(x1,V2)
rcx1 <- x1[1,rcode]
rcx2 <- x1[nrow(x1)-1,rcode]
rcx <- rcx2
lapply(vindis(vind=vind,rc=rcx,j0=irregpcode(rcx)),data.table)$n


#got this far.  looks ok.  get these 2 tables into markdown 
# have 'single slide' markdown for testing, then move to ipf1
# dataget does the get of the prep