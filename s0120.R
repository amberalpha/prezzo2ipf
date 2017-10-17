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
getgd(c('dd0120','dd0120a'))
rbc <- c('2016: Affordable suburbs outperform'='London 2016','2016: Nationally, mid-price outperforms'='National 2016','2002: Affordable suburbs outperform'='London 2002','2002: Nationally, mid-price outperforms'='National 2002','2012: Prime Central London outperforms'='London 2012','...')

ui <- fluidPage(
  tags$style(type = "text/css",
             "label { font-size: 20px; }"
  ), 
  titlePanel("Value and performance"),
  
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(condition="input.rb==['...']",
      sliderInput("radius", "radius (km from London)",
                  min = 0, max = 1000,
                  value = c(0,80))
      ,
      sliderInput('year','year',min=1996,max=2017,value=c(2016,2017),sep='')
      )
      ,
      radioButtons('rb','',choices=as.character(rbc))
    ),
    
    mainPanel(
     HTML('<style type="text/css">
            
               .span12 { font-size: 10px; line-height: 21px; }
             </style>'),
      tabsetPanel(id='tsp',selected='1',
                  tabPanel("1",
                           plotlyOutput("img1")#,height=300,width=300) do nothing
                  )
      )
      
      
      
      
      
    )
  )
)



server <- function(session,input, output) {
  
  titleReact <- reactive({
    x <- names(rbc)[match(input$rb,rbc)]
    print(x)
    x
  })
  
  observe({
     vv <- switch(input$rb,'London 2016'=c(0,80,2016,2016),'National 2016'=c(0,500,2016,2016),'London 2002'=c(0,80,2002,2002),'National 2002'=c(0,500,2002,2002),'London 2012'=c(0,80,2012,2012))
    print(vv)
    updateSliderInput(session,'radius',value=vv[1:2]) 
    updateSliderInput(session,'year',value=vv[3:4]) 
  })
  
  output$img1 <- renderPlotly({
    x1 <- dd0120a[dist<=max(input$radius)&dist>=min(input$radius),other]
    x2 <- setkey(dd0120,rcode)[x1][openforyear<=max(input$year)&openforyear>=min(input$year)]
    x3 <- ggplot(x2,aes(ppm2t,totalret,text=rcode))+geom_point(size=.4)+
      theme(axis.text.x = element_text(size=axispoint),
            axis.text.y = element_text(size=axispoint),  
            axis.title.x = element_text(size=axispoint),
            axis.title.y = element_text(size=axispoint),
            plot.title = element_text(size=22))+
    xlab('price £/m2')+ylab('calendar year return')+ggtitle(titleReact())#+ggtitle("Performance/Price scatter by postcode",subtitle=titleReact())
    ggplotly(x3,width=wx,height=wx)
    #x3
  })
  

}

shinyApp(ui = ui, server = server)

