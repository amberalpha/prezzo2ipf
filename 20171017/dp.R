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
nn<- c('scoxd','buifostnd','sty1d')
getgd(nn)

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
fsty('2000-12-31',nc=8)


#factors
fsco <- function(
  d1='1994-12-31'
  ,
  d2='2017-12-31'
  ,
  n=1
  ,
  nn='scoxd'
  ,
  label=c('The Market','Southeast and Commutable Cycle','Non-London Employment Centres Cycle')
  ) {
  getgd(nn)
  f1 <- function(o1d=gett('o1d'),n=1,colorx=c('red','green','blue')) {
    o2d <- data.table(mattotab(o1d))[,date:=as.Date(date)][,factor:=substr(bui,4,4)][,bui:=NULL]
    ggplot(data.frame(o2d[factor==n]),aes(date,field,color=factor))+geom_line(size=.8)+ scale_color_manual(values=colorx[n])+ 
      theme(axis.title.x = element_blank(),legend.position="none") + ylab('cumulative score') + ggtitle(paste0('Factor ',n,': ',label[n]))  
  }
  da <- index(scoxd)
  i <- da[da>as.Date(d1)&da<as.Date(d2)]
  setreturn(T)
  o1d <- o1(scoxd[i])
  f1(o1d,n)
}
fsco(n=2)

#attribution


fatt <- function(d1='1995-03-31',
                 nn=c('decd','dec2xd'),
                 j=c('M--30-'),
                 cc=c('red','brown','green','dark grey','blue','orange','black'),
                 ...) {
  getgd(nn)
  ii <- c(1,5,2,4,3)#seq_along(compnam())
  refdate=as.Date(d1)
  pp <- decd
  ii <- 1:6
  fatt1 <- function(o1d=gett('o1d'),addlegend=T,col=rainbow(ncol(o1d)),...) {
    o2d <- data.table(mattotab(o1d))[,date:=as.Date(date)][,postcode:=irregpcode(bui)][,bui:=NULL]
    #browser()
    print(col)
    p <- ggplot(data.frame(o2d),aes(date,field,colour=postcode))#+scale_color_manual(values=col)
    p + geom_line(size=1.) + theme(axis.title.x = element_blank()) + ylab('cumulative log return')  +ggtitle('postcode index') +
      scale_color_manual(values=col)
  } 
  compnam <- function(){c('factor 1', 'local factors', 'factor 2', 'residual', 'factor 3', 'total, f1+f2+f3')}
  #pp <- list(decd[[1]],dec2xd[[1]]+dec2xd[[2]],decd[[2]],dec2xd[[4]],decd[[3]],decd[[4]])
  #browser()
  pp <- list(
    decd$re1, #f1
    dec2xd$re1r+dec2xd$re2r, #local 1+2
    decd$re2,#f2
    dec2xd$rerr, #local residual
    decd$re3,#f3
    decd$ret #total
    )
  j <- sort(j)
  dd <- lapply(lapply(pp,'[',,j,drop=F),cumsum,...)
  yr <- range(as.numeric(unlist(dd)))
  x <- as.list(seq_along(dd))
  #browser()
  dd[[6]] <- zm(cbind(dd[[6]],dd[[1]]+dd[[3]]+dd[[5]])) #f1,2,3 fit
  for(i in seq_along(dd)) {
    colx <- cc[i]
    if(i==length(dd)) colx <- cc[i:(i+1)] #horrid
    print(colx)
    x[[i]] <- fatt1(dd[[i]],col=colx)+expand_limits(y=range(as.numeric(unlist(dd))))
  }
  x<-lapply(x,`+`,theme(legend.position="none")) 
  # x[[1]] <- x[[1]]+ 
  #   theme(legend.justification=c(1,0), legend.position=c(1,0), legend.title=element_blank(), 
  #         legend.key = element_rect(colour = NA, fill = NA))
  x[-(1:2)]<-lapply(x[-(1:2)],`+`,ylab(""))
  for(i in seq_along(x)) x[[i]]<-x[[i]]+ggtitle(compnam()[i])#+scale_fill_manual(values=cc) #+ scale_colour_manual(values=cc)
  multiplot(plotlist=x[ii],cols=3)
}

fatt(d1='1995-03-31',j='TS-2--')


#check out bd1
ggplot(dec2d[rc=='BD-1--'&ico==441&comp=='retr'][,date:=as.Date(da)][,cs:=cumsum(value)],aes(date,cs))+geom_line()
xy <- dec2d[rc=='BD-1--'&ico==441][,cs:=cumsum(value),comp][,date:=as.Date(da)]
xy[]
ggplot(xy,aes(date,cs,color=comp))+geom_line(size=.8)
