getgd(c('ldgxd','celid'))
ldgxd
ce <- celid[[length(celid)]]
x1 <- vcvce(ce)$T
n1 <- dim(x1)[1]
# x4 <- diag(n1)[-(1:3),]
# x3 <- as.matrix(ldgxd[time==2017,2:4,with=F])
# x5 <- cbind(x3,t(x4))
# x2 <- solve.QP(Amat=x5,dvec=rep(0,n1),Dmat=x1,meq=1,bvec=c(1,0,0,rep(0,n1-3)))

i1 <- 1
i2 <- 2
v1 <- rep(NA,n1)
for(i3 in 3:n1) {
  i <- c(i1,i2,i3)
  Dmat <- x1[i,i]
  dvec <- rep(0,3)
  bvec <- c(1,0,0)
  Amat <- x3[i,]
  x6 <- solve.QP(Dmat=Dmat,dvec=dvec,Amat=Amat,bvec=bvec,meq=3)$solution
  v1[i3] <- x6%*%Dmat%*%x6
  print(v1[i3])
}
min(v1,na.rm=T)

dim(x5)
n1
length(bvec)
x2$solution


dim(Amat)
dim
dim()
?solve.QP
jgetgd(celid
    )