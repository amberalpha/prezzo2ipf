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


#factor snail plot
fsn1 <- function(d1='2000-12-31',d2='2016-12-31',nn='scoxd') {
  getgd(nn)
  xy <- data.table(copy(scoxd))
  xy[,x:=cumsum(fmp2)][,y:=cumsum(fmp3)][,date:=index(scoxd)][,datex:=ifelse(.I%%8&!(.I==1)&!(.I==nrow(xy)),NA,as.character(index(scoxd)))]
  ggplot(xy[date>=as.Date(d1)&date<=as.Date(d2)],aes(x,y,label=datex))+geom_path(arrow=arrow(length=unit(0.2,'cm')))+geom_label(aes(fill=factor(datex)))+
    scale_color_brewer(palette = "Set2")+
    geom_segment(aes(xend=c(tail(x, n=-1), NA), yend=c(tail(y, n=-1), NA)),arrow=arrow(length=unit(0.2,"cm")))+
    theme(legend.position="none")+
    xlab('factor 2')+ylab('factor 3')
}
fsn1('2003-12-31')

#timeseries score-only stereotypes
require(directlabels)
rm('buifostnd')
fsty <- function(d1='2000-12-31',d2='2016-12-31',nclust=5,nn=c('buifostnd','sty1d')) {
  getgd(nn)
  rc <-  sty1d[rclust==nclust,unique(sc)]
  x0 <- unfactordt(setnames(data.table(melt(coredata(zm(buifostnd[,rc,drop=F])))),c('da','rc','ret')))[,da:=as.Date(da)]
  x1 <- x0[da>=as.Date(d1)&da<as.Date(d2)]
  xy <- x1[,y:=cumsum(ret),rc][,.(x=da,y,g=rc)]
  gg0(xy,cex=.9)+labs(x='',y='cumulative log return',title='') +
    theme_grey(base_size = 18) +
    theme(legend.position='none') #+
  #scale_colour_manual(values=scalepcselR())
}
fsty('2000-12-31',nc=12)

#factors
rm('buifostnd')
p2score(o1(gett('scoxd'),refdate=basedateR()))

o2d <- data.table(mattotab(o1d))[,date:=as.Date(date)][,factor:=substr(bui,4,4)][,bui:=NULL]
p <- ggplot(data.frame(o2d),aes(date,field,col=factor))
p + geom_line(size=1.) + theme(axis.title.x = element_blank()) + ylab('cumulative score')  +ggtitle('factor performance')

fsco <- function(n=1) {
f1 <- function(o1d=gett('o1d'),n=1,colorx=c('red','green','blue')) {
  o2d <- data.table(mattotab(o1d))[,date:=as.Date(date)][,factor:=substr(bui,4,4)][,bui:=NULL]
  ggplot(data.frame(o2d[factor==n]),aes(date,field,color=factor))+geom_line(size=.8)+ scale_color_manual(values=colorx[n])+ 
    theme(axis.title.x = element_blank(),legend.position="none") + ylab('cumulative score')  
}
o1(gett('scoxd'))
f1(o1d,3)
f1(o1d[,'fmp2',drop=F],col='green')

fsty <- function(d1='2000-12-31',d2='2016-12-31',nclust=5,nn=c('buifostnd','sty1d')) {
  getgd(nn)
  rc <-  sty1d[rclust==nclust,unique(sc)]
  x0 <- unfactordt(setnames(data.table(melt(coredata(zm(buifostnd[,rc,drop=F])))),c('da','rc','ret')))[,da:=as.Date(da)]
  x1 <- x0[da>=as.Date(d1)&da<as.Date(d2)]
  xy <- x1[,y:=cumsum(ret),rc][,.(x=da,y,g=rc)]
  gg0(xy,cex=.9)+labs(x='',y='cumulative log return',title='') +
    theme_grey(base_size = 18) +
    theme(legend.position='none') #+
  #scale_colour_manual(values=scalepcselR())
}
fsty('2000-12-31',nc=12)

