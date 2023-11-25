##### SERVER #####

server <- function(input, output, session) {
  df <- reactive({ # instanciation de la réactivité
    req(input$file1) # requête sur le fichier uploadé
    fileType <- tolower(input$format)
    
    if (fileType == ".csv") { # si fichier = csv
      return(read.csv(input$file1$datapath, # lecture du fichier csv
                      header = TRUE,
                      sep = input$separator, # avec séparateur saisi par user
                      dec = input$decimal)) # et décimale saisie par user
    } else if (fileType == ".xlsx"){ # si fichier = excel
      return(readxl::read_excel(input$file1$datapath))  # lecture du fichier excel
    } else { # sinon : pas de lecture
      return(NULL)
    }
  })
  
  # Affichage du tableau de données à partir du fichier uploadé
  output$contents <- renderDataTable({ 
    req(df()) # requête sur "df" défini plus haut (= fichier uploadé)
    if(input$show == 0) { 
      return(NULL) 
    } else { 
      head(df(), 10) 
    }
  })
  
  # Affichage des stats descriptives
  output$statsOutput <- renderPrint({ 
    req(df()) 
    if (is.data.frame(df())) { 
      cat("Descriptive Statistics:\n") 
      print(summary(df())) 
    }
  })
  
  observe({
    req(df())
    if (input$show == 0){
      return(NULL)
    } else {
      updateSelectInput(session, 'predictors', 'Select features:',
                        choices=names(df()))
      
      updateSelectInput(session, "response", "Select label:",
                        choices=names(df()),
                        selected=names(df())[1])
    }
    ##### À partir de là, ça bogue
    # Subset the data based on user inputs
    x <- df()[, input$predictors, drop = FALSE]
    y <- df()[, input$response, drop = FALSE]
    
    # Use createDataPartition to split the data
    set.seed(123)  
    trainIndex <- createDataPartition(y, p = 0.7, list = FALSE)
    xTrain <- x[trainIndex, , drop = FALSE]
    xTest <- x[-trainIndex, , drop = FALSE]
    yTrain <- y[trainIndex, , drop = FALSE]
    yTest <- y[-trainIndex, , drop = FALSE]
    
    # Update outputs
    output$xTrainPreview <- renderDataTable({
      head(xTrain, 10)
    })
    
    output$classDistribution <- renderPlot({
      par(mar = c(4, 4, 2, 2))  
      barplot(table(yTrain), main = "Class Distribution", xlab = "Class", ylab = "Frequency")
    })
  })
}

##### APP #####

shinyApp(ui, server)

