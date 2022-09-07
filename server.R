#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Deployment on shinyapps.io
# run the following commands in the console
# library(rsconnect)
# rsconnect::deployApp('path/to/your/app')
# rsconnect::deployApp('C:/Users/Kristi J/Documents/PSU_STAT/STAT485_R/Project/STAT485_Project-shiny')

library(shiny)
library(DT)
library("Hmisc")
library(shinyjs) # used for disable

# Define server logic
shinyServer(function(input, output) {
    
    hide("csvPath")
    hide("fileSel")
    
    # ++++++++++++++++++++++++++++
    # flattenCorrMatrix
    # copied from: http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software
    # ++++++++++++++++++++++++++++
    # cormat : matrix of the correlation coefficients
    # pmat : matrix of the correlation p-values
    flattenCorrMatrix <- function(cormat, pmat) {
        ut <- upper.tri(cormat)
        data.frame(
            row = rownames(cormat)[row(cormat)[ut]],
            column = rownames(cormat)[col(cormat)[ut]],
            cor  =(cormat)[ut],
            p = pmat[ut]
        )
    }
    
    buttonInput <- function(FUN, len, id, ...) {
        inputs <- character(len)
        for (i in seq_len(len)) {
            inputs[i] <- as.character(FUN(paste0(id, i), ...))
        }
        inputs
    }
    
    #-------------------------------------------------------------------
    # globals
    
    vals <- reactiveValues()                        # buttons for each table row
    printText <- reactiveValues(row = '', col = '', rsq=0) # react to table buttons
    
    # do some common data manipulation on the dataset
    
    dataFunc <- function(data)
    {
        # before rcorr can be called, need to do some data cleaning
        # remove columns where there is only 1 single value because the variance will be zero.
        t = lapply(data, function(x) length(unique(x)))
        drop = names(t[which(t <= 1)])
        data.clean = data[, !(names(data) %in% drop)] # remove these columns
        
        # remove non-numeric columns too
        l = lapply(data.clean, function(x) is.numeric(x))
        drop = names(l[which(l == FALSE)])
        data.clean = data.clean[, !(names(data.clean) %in% drop)] # remove these columns
        
        mycor = rcorr(na.omit(as.matrix(data.clean[,2:ncol(data.clean)]))) # remove rows with NA in them
        
        flat = flattenCorrMatrix(mycor$r, mycor$P)
        flat$Rsq <- round(flat$cor * flat$cor,3) # compute the r-squared value, round everything to 3 decimal places
        flat$cor <- round(flat$cor,3)
        #flat$p <- round(flat$p,3)
        flat$p <- format(flat$p, format = "e", digits = 3)
        flat <- flat[order(flat$Rsq, decreasing = TRUE),] # order in decreasing
        vals$flat = flat # set the global
        vals$clean = data.clean
        
        # ------------------------------------------------------------------
        # reactive logic
        
        # output the distribution plot
        output$plot = renderPlot(
            {
                hist(flat$Rsq
                     , main = paste(input$DataSrc, " Distribution of R-squared")
                     , xlab = "R-squared")
                legend("topright",ncol=1,legend=c(""), bty="n", title = paste("N = ", nrow(flat)))
            }
        )
        show("plot")
    }
    
    #####################################
    observeEvent(input$csvPath, # selected a .csv file
         if(file.exists(input$csvPath$datapath))
         {
             show("fileSel")
         }
    )
    
    #####################################
    observeEvent(input$fileSel, # user wants to explore their .csv file
         if(file.exists(input$csvPath$datapath))
         {
             data = read.csv(input$csvPath$datapath)
             dataFunc(data)
         }
    )
    
    #####################################
    # select data based on user input
    observe({
        if(input$DataSrc == "mtcars")
        {
            hide("csvPath")
            hide("fileSel")
            hide("table")
            hide("matrix")
            hide("txtout")
            data("mtcars")

            dataFunc(mtcars) # go ahead and run with this
        }
        else
        {
            show("csvPath")
            #show("fileSel")
            hide("plot") # hide until data is read
            hide("table")
            hide("matrix")
            hide("txtout")
        }
    })
    # ------------------------------------------------------------------
    # reactive logic
    
    #####################################
    # write text box...this updates only after user clicks the button, which is what we want
    lab <- eventReactive(input$action,
            paste("<b>Pairs where R-squared is greater than or equal to: ", input$Rsq, "(N = ", length(which(vals$flat$Rsq >= input$Rsq)), ")</b>") # make this bold

    )
    
    output$txtout = renderText({lab()})
    
    #####################################
    observeEvent(input$action,{
        show("table")
        show("txtout")
         flat = vals$flat
         # add a button to the data table
         # reference: https://community.rstudio.com/t/add-a-button-into-a-row-in-a-datatable/18651/2
         vals$Data <- data.frame(flat[which(flat$Rsq >= input$Rsq),])
         vals$Data[,"Action"] <- buttonInput(
             FUN = actionButton,
             len = length(which(flat$Rsq >= input$Rsq)),
             id = "button_",
             label = "xy plot",
             onclick = 'Shiny.onInputChange(\"lastClick\", this.id)'
         )
         
         # reference: https://stackoverflow.com/questions/35624413/remove-search-option-but-leave-search-columns-option
         output$table <- DT::renderDataTable({
             DT = vals$Data
             datatable(DT, escape = FALSE, 
                       selection = "single"    # only select one at a time
                       #,options = list(dom = 't')# remove search bar 
             ) 
         })
    })
    
    # ----------------------------------------
    # user clicks a specific row button, update some values to be used below in the xy plot
    observeEvent(input$lastClick, {
        selectedRow <- as.numeric(strsplit(input$lastClick, "_")[[1]][2])
        printText$row <<- vals$Data[selectedRow,1]
        printText$col <<- vals$Data[selectedRow,2]
        printText$rsq <<- vals$Data[selectedRow,c("Rsq")]
    })
    
    
    # don't do these things until the button click
    observeEvent(input$lastClick,{
        output$matrix <- renderPlot({
            # generate the xy plot for the selected row from the table
            row = as.character(printText$row)
            col = as.character(printText$col)
            
            if (length(row) > 0){
                x = vals$clean[,c(row)]
                y = vals$clean[,c(col)]
                #par(mfrow=c(1,2))
                #plot1 <- xyplot(vals$clean[,c(col)] ~ vals$clean[,c(row)], 
                #       , xlab = list(row, cex=1.5), ylab = list(col, cex=1.5)# make the labels bigger
                #       , main = paste("Rsq = ", round(printText$rsq,3))
                #       , type = c("p", "r") # add points and regression line
                #       , jitter.data = TRUE, pch = 19 # filled dots
                #       #, panel = panel.smoothScatter
                #)
                #    
                smoothScatter(x, y
                              , xlab = list(row, cex=1.5), ylab = list(col, cex=1.5)# make the labels bigger
                              , main = paste("Rsq = ", round(printText$rsq,3))
                              )
                abline(lm( y ~ x))
                
                # print side by side lattice plots
                #print(a, position = c(0.5,0,1,1), more = TRUE)
                #print(b, position = c(0.5,0,1,1))
            }
        })
        show("matrix")
    })
    
    
})
