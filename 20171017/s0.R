# app <<- 'ppd'
# ver <<- 45
# require(leaflet)
# library(shiny)
# require(aappd)
# source('init.R')
# aatopselect('aappd')
# wx <- 800
# axispoint <- 16
# getgd(c('dd0120','dd0120a','dd140a','xx0150b'))
# 
# rbc0120 <- c('2016: Affordable suburbs outperform'='London 2016','2016: Nationally, mid-price outperforms'='National 2016','2002: Affordable suburbs outperform'='London 2002','2002: Nationally, mid-price outperforms'='National 2002','2012: Prime Central London outperforms'='London 2012','...')
# rbc0140 <- c('1995-2001','2001-2005','2005-2014','2014-?')
# rbc0150 <- c('1995-2001','2001-2005','2005-?')

ui <- fluidPage(

  titlePanel("title"),
  
  sidebarLayout(
    sidebarPanel(

    ),
    
    mainPanel(
     
      tabsetPanel(id='tsp',selected='0110',
                  tabPanel("0110",
                           includeMarkdown('s0110.Rmd')
                  )
      )
      
      
      
      
      )
  )
  )



server <- function(session,input, output) {
  

  }


shinyApp(ui = ui, server = server)

