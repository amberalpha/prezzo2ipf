#20171015 works just fine in both preview and chrome
#suspendwhenhidden=F makes it load in background
#both server and ui are verbose 
#ui may be inevitable
#server should be done in loop but not works today
app <<- 'ppd'
ver <<- 45
require(leaflet)
library(shiny)
require(aappd)
source('init.R')
aatopselect('aappd')
wx <- 800
axispoint <- 16
getgd(c('dd0120','dd0120a','dd140a'))

rbc140 <- c('1995-2001','2001-2005','2005-2014','2014-?')

ui <- fluidPage(
  tags$style(type = "text/css",
             "label { font-size: 20px; }"
  ), 
  titlePanel("Divergence and convergence"),
  
  sidebarLayout(
    sidebarPanel(
      

      radioButtons('rb','',choices=as.character(rbc140))
    ),
    
    mainPanel(
      tabsetPanel(id='tsp',selected='1',
                  tabPanel("1",
                           plotlyOutput("img1")#,height=300,width=300) do nothing
                  )
      )
    )
  )
)



server <- function(session,input, output) {
  
  # titleReact <- reactive({
  #   x <- names(rbc)[match(input$rb,rbc)]
  #   print(x)
  #   x
  # })
  # 
  # observe({
  #    vv <- switch(input$rb,'London 2016'=c(0,80,2016,2016),'National 2016'=c(0,500,2016,2016),'London 2002'=c(0,80,2002,2002),'National 2002'=c(0,500,2002,2002),'London 2012'=c(0,80,2012,2012))
  #   print(vv)
  #   updateSliderInput(session,'radius',value=vv[1:2]) 
  #   updateSliderInput(session,'year',value=vv[3:4]) 
  # })
  
  output$img1 <- renderPlotly({
    #browser()
    i <- match(input$rb,rbc140)
    tp1 <- c('2001-12-31','2005-12-31','2014-09-30','2017-12-31')
    tp2 <- as.Date(tp1)
    yrange <- dd140a[,range(cr,na.rm=T)]
    x2 <- copy(dd140a)[date>tp2[i],cr:=NA]
    x3 <- ggplot(x2,aes(date,cr)) + 
      geom_line(aes(color=rc))+
      ylim(yrange) + 
      ylab('cumulative log return') + xlab('') +
      theme(axis.text.x = element_text(size=axispoint),
            axis.text.y = element_text(size=axispoint),  
            axis.title.x = element_text(size=axispoint),
            axis.title.y = element_text(size=axispoint),
            plot.title = element_text(size=22))+
      theme(legend.title=element_blank())
      #theme(legend.position=c(0,1))
    
      
    ggplotly(x3,width=wx*1.2,height=wx)
  })
  

}

shinyApp(ui = ui, server = server)

