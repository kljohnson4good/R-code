#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyFiles) # required for file picker
library(shinyjs)    # required to hide/show elements


# Define UI for application that draws a histogram
shinyUI(fluidPage(
    useShinyjs(), # setup shiny js for disable/enable functionality to work
    # Application title
    titlePanel("User Defined Input"),

    # top panel with an input box for the user to define the output
    # place the button to the right of the input
    wellPanel(
        fluidRow(
            column(4, # width is 4 of 12 units
                   selectInput('DataSrc', "Select Data",
                               c("mtcars", "select .csv"))
            ),
            column(2, # width is 3 of 12 units
                numericInput('Rsq', 'R-squared limit', 0.7,
                             min = 0, max = 1, step = 0.01, width=125) # width in px
            ),
            column(2, 
                br(), # add a spacer for vertical alignment
                # add an action button
                actionButton(inputId = "action", label = "Perform Analysis")
            )
        ) # end of row 1
        ,
        fluidRow(
                column(8, fileInput("csvPath", "upload user-defined .csv file",
                                 accept = c("csv", ".csv"))
                ),
                column(4, br(), # add a spacer for vertical alignment
                       actionButton(inputId = "fileSel", "Explore csv")
                )
        ) # end of row 2
        
    ), # end of top panel
        
    # Main layout 
    mainPanel(
        plotOutput("plot") # plot the distribution of R-squared values
        ,htmlOutput("txtout") # use html instead of textOutput for easier formatting like bold
        ,DT::dataTableOutput("table")
        ,plotOutput("matrix") # xy plot matrix
    )
))
