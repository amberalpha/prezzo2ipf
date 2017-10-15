#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
#source('../prezzo2ipf/ipf1dataget.R') 
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$map1 <- renderLeaflet({
    cl1(dezo2d,dex=names(dezo2d@data)[1],width='450px',height='900px')
  })
  output$map 2<- renderLeaflet({
    cl1(dezo2d,dex=names(dezo2d@data)[2],width='450px',height='900px')
  })
  output$map3 <- renderLeaflet({
    cl1(dezo2d,dex=names(dezo2d@data)[3],width='450px',height='900px')
  })
  
})
