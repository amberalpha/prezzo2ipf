app <<- 'ppd'
ver <<- 45
require(leaflet)
library(shiny)
require(aappd)
source('init.R')
aatopselect('aappd')
wx <- 800
axispoint <- 16
getgd(c('dd0120','dd0120a','dd140a','xx0150b'))

rbc0120 <- c('2016: Affordable suburbs outperform'='London 2016','2016: Nationally, mid-price outperforms'='National 2016','2002: Affordable suburbs outperform'='London 2002','2002: Nationally, mid-price outperforms'='National 2002','2012: Prime Central London outperforms'='London 2012','...')
rbc0140 <- c('1995-2001','2001-2005','2005-2014','2014-?')
rbc0150 <- c('1995-2001','2001-2005','2005-?')

ui <- fluidPage(
  tags$style(type = "text/css",
             "label { font-size: 20px; }"
  ), 
  titlePanel("Residential: capital return"),
  
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(condition="input.txxx==['cycles']",
                       sliderInput("radius", "radius (km from London)",
                                   min = 0, max = 1000,
                                   value = c(0,80))
                       ,
                       sliderInput('year','year',min=1996,max=2017,value=c(2016,2017),sep='')
                       ,
                       conditionalPanel(condition="input.t010x==['scatter']",radioButtons('rb0120','',choices=as.character(rbc0120)))
                       ,
                       conditionalPanel(condition="input.t010x==['kensington']",radioButtons('rb0140','',choices=as.character(rbc0140)))
                       ,
                       conditionalPanel(condition="input.t010x==['dispersion']",radioButtons('rb0150','',choices=as.character(rbc0150)))
      )
    )
    ,
    mainPanel(
      tabsetPanel(id='txxx',selected='content',
                  tabPanel("content",
                           includeMarkdown('s0010.Rmd')
                  )
                  ,
                  tabPanel("cycles",
                           tabsetPanel(id="t010x",selected='scatter',
                                       tabPanel("scatter",
                                                plotlyOutput("img0120")#,height=300,width=300) do nothing
                                       )
                                       ,
                                       tabPanel("kensington",
                                                plotlyOutput("img0140")#,height=300,width=300) do nothing
                                       )
                                       ,
                                       tabPanel("dispersion",
                                                plotlyOutput("img0150")#,height=300,width=300) do nothing
                                       )
                           )
                  )
      )
    )
  )
)
  



server <- function(session,input, output) {
  
  t0120R <- reactive({
    x <- names(rbc0120)[match(input$rb0120,rbc0120)]
    print(x)
    x
  })
  
  observe({
    vv <- switch(input$rb0120,'London 2016'=c(0,80,2016,2016),'National 2016'=c(0,500,2016,2016),'London 2002'=c(0,80,2002,2002),'National 2002'=c(0,500,2002,2002),'London 2012'=c(0,80,2012,2012))
    print(vv)
    updateSliderInput(session,'radius',value=vv[1:2]) 
    updateSliderInput(session,'year',value=vv[3:4]) 
  })
  
  output$img0120 <- renderPlotly({
    x1 <- dd0120a[dist<=max(input$radius)&dist>=min(input$radius),other]
    x2 <- setkey(dd0120,rcode)[x1][openforyear<=max(input$year)&openforyear>=min(input$year)]
    x3 <- ggplot(x2,aes(ppm2t,totalret,text=rcode))+geom_point(size=.4)+
      theme(axis.text.x = element_text(size=axispoint),
            axis.text.y = element_text(size=axispoint),  
            axis.title.x = element_text(size=axispoint),
            axis.title.y = element_text(size=axispoint),
            plot.title = element_text(size=22))+
      xlab('price £/m2')+ylab('calendar year return')+ggtitle(t0120R())#+ggtitle("Performance/Price scatter by postcode",subtitle=t0120R())
    ggplotly(x3,width=wx,height=wx)
    #x3
  })
  
  output$img0140 <- renderPlotly({
    #browser()
    i <- match(input$rb0140,rbc0140)
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
  
  output$img0150 <- renderPlotly({
    #browser()
    x1 <- xx0150b[variable=='75/25']
    i <- match(input$rb0150,rbc0140)
    tp1 <- c('2001-12-31','2005-12-31','2017-12-31')
    tp2 <- as.Date(tp1)
    yrange <- x1[,range(value,na.rm=T)]
    x2 <- copy(x1)[date>tp2[i],value:=NA]
    x3 <- ggplot(x2[grepl('^75/25',variable)],aes(date,value)) + 
      geom_line(aes(color=variable),size=.7) + 
      ylab('relative £/m2, quartile 4/quartile 1') + xlab('')+
      theme(axis.text.x = element_text(size=axispoint),
            axis.text.y = element_text(size=axispoint),  
            axis.title.x = element_text(size=axispoint),
            axis.title.y = element_text(size=axispoint),
            plot.title = element_text(size=22))+
      theme(legend.position="none")+ggtitle('National dispersion of £/m2')
    
    
    
    ggplotly(x3,width=wx*1.2,height=wx)
    
  })
}

shinyApp(ui = ui, server = server)

