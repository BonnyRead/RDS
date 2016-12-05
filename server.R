
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(RGA)
library(data.table)

shinyServer(function(input, output) {
    
  authorize()
  
  datafile <- reactive({
    get_ga(profileId = "91337451",start.date = input$querydate[1],
           end.date = input$querydate[2],metrics = "ga:itemRevenue,
           ga:uniquePurchases,ga:buyToDetailRate",dimensions = "ga:productName")
  })
  
  output$pikachu <- renderDataTable({
    datafile()
  })

})
