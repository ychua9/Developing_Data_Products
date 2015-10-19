# The user-interface definition of the Shiny web app.
library(shiny)
library(BH)
library(rCharts)
require(markdown)
require(data.table)
library(dplyr)
library(DT)

shinyUI(
  navbarPage("Singapore Population Visualizer", 
             # multi-page user-interface that includes a navigation bar.
             tabPanel("Explore the Data",
                      sidebarPanel(
                        sliderInput("timeline", 
                                    "Timeline:", 
                                    min = 1960,
                                    max = 2014,
                                    value = c(1996, 2014)),
                        uiOutput("popVariables"),
                        actionButton(inputId = "clearAll", 
                                     label = "Clear selection", 
                                     icon = icon("square-o")),
                        actionButton(inputId = "selectAll", 
                                     label = "Select all", 
                                     icon = icon("check-square-o"))                        
                      ),
                      mainPanel(
                        tabsetPanel(
                          # Data 
                          tabPanel(p(icon("table"), "Dataset"),
                                   dataTableOutput(outputId="dTable")
                          ), # end of "Dataset" tab panel
                         tabPanel(p(icon("line-chart"), "Visualize the Data"),
#                                    h4('Total Population (Number)', align = "center"),
                                   showOutput("popVar1", "polycharts"),#"nvd3"),
#                                    h4('Total Population Growth (Percent)', align = "center"),
                                   showOutput("popVar2", "nvd3"),
#                                    h4('Resident Population Growth (Percent)', align = "center"),
                                   showOutput("popVar3", "nvd3")

                          ) # end of "Visualize the Data" tab panel
                        )
                      )     
             ), # end of "Explore Dataset" tab panel
             tabPanel("About",
                      mainPanel(
                        includeMarkdown("about.md")
                      )
             ) # end of "About" tab panel
  )
  
)