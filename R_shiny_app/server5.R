library(rsample)

# chargement du dataset :
data <- read.csv("/Users/annabellenarsama/Desktop/SISE/Programmation_R/données/mon_dataset.csv")
print(data)

# train-test split :
set.seed(1) # reproducibilité !!!
split <- initial_split(data, prop = 0.7, strata = "sexe") # rsample : stratif° par label
data_train <- training(split)
data_test <- testing(split)

# split des prédicteurs (X) et du label (y) :
X_train <- subset(data_train, select = -sexe)
class(X_train) # df
dim(X_train) # (397, 30)

y_train <- data_train[, "sexe"]
class(y_train) # character (vecteur)

X_test <- subset(data_test, select = -sexe)
class(X_test) # df
dim(X_test) # (172, 30)

y_test <- data_test[, "sexe"]
class(y_test) # character (vecteur)

##### SERVER #####

install.packages("devtools")
library(devtools)

devtools::install_github('Abdouragit/GaussianNaiveBayes')
library(GaussianNaiveBayes)

server <- function(input, output, session) {
  
  df <- reactive({
    req(input$file1)
    fileType <- tolower(input$format)
    
    if (fileType == ".csv") {
      return(read.csv(input$file1$datapath, header = TRUE, sep = input$separator, dec = input$decimal))
    } else if (fileType == ".xlsx") {
      return(readxl::read_excel(input$file1$datapath))
    } else {
      return(NULL)
    }
  })
  
  output$contents <- renderDataTable({
    req(df())
    if (input$show == 0) {
      return(NULL)
    } else {
      head(df(), 10)
    }
  })
  
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
  
  X <- reactiveVal(NULL)
  y <- reactiveVal(NULL)
  X_train <- reactiveVal(NULL)
  y_train <- reactiveVal(NULL)
  X_test <- reactiveVal(NULL)
  y_test <- reactiveVal(NULL)
  
  observeEvent(input$updateTrainTest, {
    
    X(df()[, input$predictors, drop = FALSE])
    y(df()[, input$response, drop = FALSE])
    
    set.seed(1)
    
    indices_stratified <- split(1:nrow(df()), y())
    train_size <- round(0.8 * nrow(df()))
    train_indices <- unlist(lapply(indices_stratified, function(x)
      sample(x, size = round(train_size * length(x)/nrow(df())))))
    
    test_indices <- setdiff(1:nrow(df()), train_indices)
    
    train_data <- df()[train_indices, ]
    test_data <- df()[test_indices, ]
    
    X_train(train_data[, input$predictors, drop=FALSE])
    y_train(train_data[, input$response])
    
    X_test(test_data[, input$predictors, drop=FALSE])
    y_test(test_data[, input$response])
    
    output$xTrainPreview <- renderTable({
      head(df()[, input$predictors], 10)
    })
    
    output$classDistribution <- renderPlot({
      freq_table <- table(y_train())/sum(table(y_train()))
      barplot(freq_table)
    })
    
    NB <- reactiveVal(Gaussian_Naive_Bayes$new())
    
    y_pred <- reactiveVal(NULL)
    fit_model <- reactiveVal(NULL)
    
    observeEvent(input$trainModel, {
      
      fit_model(NB()$fit(X_train(), y_train()))
      
      output$parameters <- renderPrint({
        return(NB()$params)
      })
      
      results <- print(NB())
      output$probabilities <- renderPrint({
        return(results)
      })
      
      #summary <- NB$summary()
      output$summary <- renderPrint({
        return(NB()$summary())
      })
    })
    
    observeEvent(input$predictButton, {
      
      y_pred(NB()$predict(X_test(), threshold = 0.8, eps = 0))
      output$posteriors <- renderPrint({
        return(y_pred())
      })
      
      y_proba <- NB()$predict_proba(X_test())
      output$yPred <- renderPrint({
        return(y_proba)
      })
    })
    
    observeEvent(input$evaluate, {
      
      observed_values <- y_test()
      predicted_values <- y_pred()
      
      output$mc <- renderPrint({
        return(NB()$confusion_matrix(observed_values, predicted_values))
      })
      
      perf <- length(y_test()[NB()$predict(X_test(), threshold = 0.8, eps = 0) == y_test()]) / length(y_test())
      output$performance <- renderPrint({
        return(perf)
      })
    })
    
  })
  
}

##### APP #####

shinyApp(ui, server)


