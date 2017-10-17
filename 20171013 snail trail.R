require(aappd)
getgd('scoxd')
xy <- data.table(copy(scoxd))
xy[,x:=cumsum(fmp2)][,y:=cumsum(fmp3)][,date:=ifelse(.I%%8&!(.I==1)&!(.I==nrow(xy)),NA,as.character(index(scoxd)))]
ggplot(xy,aes(x,y,label=date))+geom_path(arrow=arrow(length=unit(0.2,'cm')))+geom_label(aes(fill=factor(date)))+
  scale_color_brewer(palette = "Set2")+
  geom_segment(aes(xend=c(tail(x, n=-1), NA), yend=c(tail(y, n=-1), NA)),arrow=arrow(length=unit(0.2,"cm")))

require(googleVis)
xy1 <- data.frame(copy(xy)[,.(x,y)][,id:='id'][,timevar:=paste0(format(dd, '%Y'),'Q',1:4)][,size:=.1])
g1 <- gvisMotionChart(xy1,id='id',time='timevar',x='x',y='y',size='size',color='')
plot(g1)

xy2 <- xy1[seq(from=4,to=88,by=4),]
xy2 <- data.frame(data.table(xy2)[,timevar:=(1995:2016)])
plot(gvisMotionChart(xy2,id='id',time='timevar',x='x',y='y'))

M2 <- gvisMotionChart(Fruits, idvar="Fruit", timevar="Date",
                      date.format = "\\%Y\\%m\\%d") 
plot(M2)

dd <- index(scoxd)
paste0(format(dd, '%Y'),'Q',1:4)
?DateTimeClasses



