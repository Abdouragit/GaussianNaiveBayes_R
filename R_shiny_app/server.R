##### SERVER #####

server <- function(input, output) {
  #data <- reactive({
    output$contents <- renderDataTable({
      req(input$file1)
      fileType <- tolower(input$format)
      if (input$show == 0){
        return(NULL)
      } else if (fileType == ".csv") {
        return(read.csv(input$file1$datapath,
                        header=TRUE,
                        sep=input$separator,
                        dec=input$decimal))
      } else if (fileType == ".xlsx"){
        return(readxl::read.xlsx(input$file1$datapath)) 
      }
    })
    
    output$statsOutput <- renderPrint({
      req(input$file1)
      if (is.character(input$file1)) {
        cat("Frequency Table:\n")
        print(table(input$file1))
      } else {
        cat("Descriptive Statistics:\n")
        print(summary(input$file1))
      }
    })
    
  #})
  
  #output$contents <- renderDataTable({
  #if(input$show == 0) {
  #return(NULL)
  #} else {
  #head(data, 10)
  #}
  #})
  
  observeEvent(input$updateTrainTest, {
    
    updateSelectInput('predictors', "Select features:",
                      choices = colnames(input$file1),
                      multiple = TRUE
    )
    updateSelectInput('response', "Select label:",
                      choices=colnames(input$file1),
                      selected=colnames(input$file1)[1])
    
    # Subset the data based on user inputs
    x <- data[, input$predictors, drop = FALSE]
    y <- data[, input$response, drop = FALSE]
    
    # Use createDataPartition to split the data
    set.seed(123)  # Pour la reproductibilité
    trainIndex <- createDataPartition(y, p = 0.7, list = FALSE)
    xTrain <- x[trainIndex, , drop = FALSE]
    xTest <- x[-trainIndex, , drop = FALSE]
    yTrain <- y[trainIndex, , drop = FALSE]
    yTest <- y[-trainIndex, , drop = FALSE]
    
    # Update outputs
    output$xTrainPreview <- renderTable({
      head(xTrain, 10)
    })
    
    output$classDistribution <- renderPlot({
      par(mar = c(4, 4, 2, 2))  # Réglages des marges
      barplot(table(yTrain), main = "Class Distribution", xlab = "Class", ylab = "Frequency")
    })
  })
}

##### APP #####

shinyApp(ui, server)
