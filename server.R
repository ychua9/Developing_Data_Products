library(shiny)

# Load data processing file
source("data_processing.R")
popVariables <- c("Total Population (Number)"="2", "Resident Population (Number)"="3", "Singapore Citizen Population (Number)"="4",
                  "Permanent Resident Population (Number)"="5", "Non-resident Population (Number)"="6",  
                  "Population Density (Total Population Per Sq Km)"="9",  "Sex Ratio: Males Per 1,000 Females"="10",
                  "Median Age Of Resident Population (Years)"="11", 
                  "Support Ratio: Persons Aged 20-64 Yrs Per Elderly Aged 65 Yrs & Over (Per Elderly)"="13",
                  "Dependency Ratio: Persons Aged Under 20 Yrs And 65 Yrs & Over Per 100 Population Aged 20-64 Yrs"="15",
                  "Old-age Dependency Ratio: Persons Aged 65 Yrs & Over Per 100 Population Aged 20-64 Yrs"="16",
                  "Natural Increase (Number)"="17")

# Shiny server
shinyServer(
  function(input, output) {
    # Initialize reactive values
    values <- reactiveValues()
    values$popVariables <- popVariables
    
    # Create event type checkbox
    output$popVariables <- renderUI({
      checkboxGroupInput('popVariables', 'Population Variables:',
                         popVariables, selected = values$popVariables)
    })
    
    # Add observer on select-all button
    observe({
      if(input$selectAll == 0) return()
      values$popVariables <- popVariables
    })
    
    # Add observer on clear-all button
    observe({
      if(input$clearAll == 0) return()
      values$popVariables <- c() # empty list
    })
    
    # Prepare dataset
    dataTable <- reactive({
      filterByYear(data, input$timeline[1],
                   input$timeline[2], input$popVariables)
    })
    
    dataTableByPopVar <- reactive({
      dataByPopVar(data, input$timeline[1], 
                     input$timeline[2], input$popVariables)
    })
    
    dataTableByPopVarPercent <- reactive({
      dataByPopVarPercent(data, input$timeline[1], 
                   input$timeline[2])
    })
    
    # Render data table
    output$dTable <- renderDataTable({
      dataTable()
    } #, options = list(bFilter = FALSE, iDisplayLength = 50)
    )
    
    output$popVar1 <- renderChart({
      plotPopVar1(dataTableByPopVar())
    })
    
    output$popVar2 <- renderChart({
      plotPopVar2(dataTableByPopVarPercent())
    })
    
    output$popVar3 <- renderChart({
      plotPopVar3(dataTableByPopVarPercent())
    })    
  } # end of function(input, output)
)