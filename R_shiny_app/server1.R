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
  })
  
  observeEvent(input$updateTrainTest, {
    
    # Subset the data based on user inputs
    X <- df()[, input$predictors, drop = FALSE]
    y <- df()[, input$response, drop = FALSE]
    
    set.seed(1)  # Pour la reproductibilité
    
    # Strata
    indices_stratified <- split(1:nrow(df()), y)
    
    # Sample size
    train_size <- round(0.8 * nrow(df()))
    
    # Train sample
    train_indices <- unlist(lapply(indices_stratified, function(x)
      sample(x, size = round(train_size * length(x)/nrow(df())))))
    
    # Test sample
    test_indices <- setdiff(1:nrow(df()), train_indices)
    
    # Train-test split
    train_data <- df()[train_indices, ]
    test_data <- df()[test_indices, ]
    
    # X-y train-test split
    X_train <- train_data[, -which(names(train_data) == input$response)]
    y_train <- train_data$y
    
    X_test <- test_data[, -which(names(test_data) == input$response)]
    y_test <- test_data$y
    
    # Update outputs
    output$xTrainPreview <- renderTable({
      head(df()[, input$predictors], 10)
    })
    
    #output$classDistribution <- renderPlot({
    #par(mar = c(4, 4, 2, 2))  # Réglages des marges
    #freq_table <- table(y_train)/sum(table(y_train))
    #max_freq <- max(freq_table)
    #xlim = c(min(X_train), max(X_train))
    #barplot(freq_table,
    #main = "Class Distribution",
    #xlab = "Class",
    #ylab = "Frequency",
    #xlim = length(freq_table), # erreur
    #ylim = c(0, max_freq))
    
  })
  
  # Création d'une variable réactive pour stocker le modèle entraîné
  trained_model <- reactiveVal(NULL)
  
  observeEvent(input$trainModel, {
    # Quand le bouton "Train Model" est cliqué
    gnb <- Gaussian_Naive_Bayes$new()
    
    # Entraîner le modèle
    fit <- gnb$fit(X_train, y_train)
    
    # Stocker le modèle dans trained_model
    trained_model(fit)
    
    # Afficher les moyennes et écart-types
    output$parameters <- renderPrint({
      print(fit)
    })
    
    # Afficher les probabilités a priori et conditionnelles
    results <- print(gnb)
    output$probabilities <- renderPrint({
      print(results)
    })
    
    # Afficher le résumé du modèle
    summary <- gnb$summary()
    output$summary <- renderPrint({
      print(summary)
    })
  })
  
  observeEvent(input$predictButton, {
    # Quand le bouton "Predict" est cliqué
    gnb <- trained_model() # Récupérer le modèle entraîné depuis trained_model
    
    # Afficher les prédictions
    y_pred <- gnb$predict(X_test, threshold = 0.8, eps = 0)
    output$posteriors <- renderPrint({
      print(y_pred)
    })
    
    # Afficher les probabilités prédites
    y_proba = gnb$predict_proba(X_test)
    output$yPred <- renderPrint({
      print(y_proba)
    })
  })
  
  observeEvent(input$evaluate, {
    # Quand le bouton "Evaluate" est cliqué
    gnb <- trained_model() # Récupérer le modèle entraîné depuis trained_model
    
    # Matrice de confusion
    matrix = gnb$confusion_matrix(y_test, y_pred)
    output$mc <- renderPrint({
      print(matrix)
    })
    
    # Performance
    perf <- length(y_test[y_pred == y_test]) / length(y_test)
    output$performance <- renderPrint({
      print(perf)
    })
  })
  
}

##### APP #####

shinyApp(ui, server)

