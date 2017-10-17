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
getgd(c('dd0120','dd0120a','dd140a','xx0150b','scoxd'))

rbc160 <- c('1995-2001','2001-2005','2005-2014','2014-?')

ui <- fluidPage(
  tags$style(type = "text/css",
             "label { font-size: 20px; }"
  ), 
  titlePanel("Cyclicality"),
  
  sidebarLayout(
    sidebarPanel(
      

      radioButtons('rb','',choices=as.character(rbc140))
    ),
    
    mainPanel(
      tabsetPanel(id='tsp',selected='1',
                  tabPanel("1",
                           plotlyOutput("img1")
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
    x1 <- melt(data.table(coredata(scoxd))[,date:=index(scoxd)],id.vars='date')[,value:=cumsum(value),variable]
    i <- match(input$rb,rbc140)
    tp1 <- c('2001-12-31','2005-12-31','2014-09-30','2017-12-31')
    tp2 <- as.Date(tp1)
    yrange <- x1[variable=='fmp3',range(value,na.rm=T)]
    x2 <- setkey(unfactordt(copy(x1)[date>tp2[i],value:=NA]),variable,date)#[,x:=NULL]
    dd <- index(scoxd)
    dlabel <- c(min(dd),dd[format(dd,'%m')=='12'],max(dd))
    x2[,x:=date]
    x2[!(x%in%dlabel),x:=as.Date(NA)]
    x2[,y:=value][,alldate:=date][,date:=x]
    
    # x1 <- data.table(copy(scoxd))[,alldate:=index(scoxd)]
    # x2 <- copy(x1)[date>tp2[i],fmp1:=NA][date>tp2[i],fmp2:=NA][date>tp2[i],fmp3:=NA]
    #xy[,x:=cumsum(fmp2)][,y:=cumsum(fmp3)][,date:=ifelse(.I%%8&!(.I==1)&!(.I==nrow(xy)),NA,as.character(index(scoxd)))]
    ggplot(x2,aes(date,value,label=date))+geom_path(arrow=arrow(length=unit(0.2,'cm')))+geom_label(aes(fill=factor(date)))+
      scale_color_brewer(palette = "Set2")+
      geom_segment(aes(xend=c(tail(x, n=-1), NA), yend=c(tail(y, n=-1), NA)),arrow=arrow(length=unit(0.2,"cm")))
    ggplotly(x3,width=wx*1.2,height=wx)

  })
  

}

shinyApp(ui = ui, server = server)

