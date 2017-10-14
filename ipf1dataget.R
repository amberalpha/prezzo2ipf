knitr::opts_chunk$set(echo = F)
require(aappd)
setreturn(F)
ver <- 41
app <- 'ppd'
source('../pkg/script/init.R')

aatopselect('aappd')
source('../pkg/R/step.R')
source('../pkg/R/lib.R')
#getlast <- gett
save(ver,file='ver.RData')
d0 <- '2007-12-31'
rcx <- '.'

getgd(c('pvrcd','hvrcd','ppm2td','dezo2d'))
getgd(c('xx020','xx030a','xx030b','xx040d','xx041d','xx050d','xx060d'))
