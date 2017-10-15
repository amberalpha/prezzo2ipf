#20171015 works just fine in both preview and chrome
#suspendwhenhidden=F makes it load in background
#both server and ui are verbose 
#ui may be inevitable
#server should be done in loop but not works today
require(leaflet)
source('ipf1dataget.R') #problem with filepaths either here or inside the script 20171011
library(shiny)
nmap <- 6
nm <- setdiff(1:nmap,3:4)

ui <- fluidPage(
  
  titlePanel("map"),
  
  sidebarLayout(
    sidebarPanel(
    ),
    
    mainPanel(
      
      tabsetPanel(id='mappanel',selected='1',
                  tabPanel("1",
                           leafletOutput("map1",height=900)
                  )
                  ,
                  tabPanel("2",
                           leafletOutput("map2",height=900)
                  )
                  ,
                  # tabPanel("3",
                  #          leafletOutput("map3",height=900)
                  # )
                  # ,
                  # tabPanel("4",
                  #          leafletOutput("map4",height=900)
                  # )
                  # ,
                  tabPanel("5",
                           leafletOutput("map5",height=900)
                  )
                  ,
                  tabPanel("6",
                           leafletOutput("map6",height=900)
                  )
      )
      
      
      
      
      
    )
  )
)



server <- function(session,input, output) {
  # for(i in 1:nmap) {
  # do.call(`$<-`,list(x=output,name=paste0('map',i),value=renderLeaflet({
  #   cl1(dezo2d,dex=names(dezo2d@data)[i],width='450px',height='900px')
  # })))
  # }#not works, was working before
  output$map1 <- renderLeaflet({
    cl1(dezo2d,dex=names(dezo2d@data)[1],width='450px',height='900px')
  })
  output$map2<- renderLeaflet({
    cl1(dezo2d,dex=names(dezo2d@data)[2],width='450px',height='900px')
  })
  # output$map3<- renderLeaflet({
  #   cl1(dezo2d,dex=names(dezo2d@data)[3],width='450px',height='900px')
  # })
  # output$map4<- renderLeaflet({
  #   cl1(dezo2d,dex=names(dezo2d@data)[4],width='450px',height='900px')
  # })
  output$map5<- renderLeaflet({
    cl1(dezo2d,dex=names(dezo2d@data)[5],width='450px',height='900px')
  })
  output$map6<- renderLeaflet({
    cl1(dezo2d,dex=names(dezo2d@data)[6],width='450px',height='900px')
  })
  #outputOptions(output, "map2", suspendWhenHidden = FALSE)
  for(i in nm) {
    outputOptions(output, paste0('map',i), suspendWhenHidden = FALSE)
  }
  
}

shinyApp(ui = ui, server = server)

