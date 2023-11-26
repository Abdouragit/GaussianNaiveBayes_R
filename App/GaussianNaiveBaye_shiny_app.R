
##### GAUSSIAN NAIVE BAYES FONCTION #####
library(GaussianNaiveBayes)

##### SHINY APP #####
library(shiny)
library(DT)
library(shinydashboard)
library(ggplot2)
library(caret)


##### USER INTERFACE #####

ui <- dashboardPage(
  skin='purple',
  dashboardHeader(title = "Gaussian Naive Bayes Gaussian"),
  dashboardSidebar(
    sidebarMenu(
      # On crée des widgets dans la barre à gauche
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Data Uploading", tabName = "import", icon = icon("file-import")),
      menuItem("Statistics", tabName = "statistics", icon = icon("chart-bar")),
      menuItem("Train/Test", tabName = "traintest", icon = icon("sliders-h")),
      menuItem("Train Model", tabName = "model", icon = icon("cogs")),
      menuItem("Predictions", tabName = "predictions", icon = icon("cogs")),
      menuItem("Model's Evaluation", tabName = "evaluation", icon = icon("cogs"))
    )
  ),
  # Dans le menu principal :
  dashboardBody(
    # on crée des onglets associés aux widgets à gauche
    tabItems(
      # Home tab
      tabItem(tabName = "home",
              fluidRow(
                box(
                  title = "Gaussian Naïve Bayes Package Interface",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  tags$div(
                    HTML("<p>The objective of this project was to create an R package that offers 
                    a GaussianNaiveBayes method for supervised classification, coded in the R6 class. 
                    This package incorporates a parallelization method to reduce computation time and accommodate 
                    large volumes of data. It can be directly installed from GitHub. 
                    </p>
                    <p>You can find all the informations about our project on this page: https://github.com/Abdouragit/GaussianNaiveBayes.git</p>
                    <p>Annabelle Narsama, Adbourahmane Ndiaye, Natacha Perez.</p>"),
                    style = "margin: 20px;")
                )
              )
      ),
      
      # Import Data tab
      tabItem(tabName = "import",
              fluidRow(
                # 1ère box
                box(
                  title = "File Upload",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  # Choix du format de fichier à uploader
                  selectInput("format", "Choose an option for file format:",
                              choices = c(".csv", ".xlsx")
                  ),
                  # Upload d'un fichier de données
                  fileInput("file1", "Choose File",
                            accept = c(".csv", ".xlsx")
                  ),
                  # Choix du séparateur dans le fichier
                  selectInput("separator", "Choose separator character:",
                              choices = c(",", ";", "tab")
                  ),
                  # Choix du caractère décimal dans le fichier
                  selectInput("decimal", "Choose character for decimal points:",
                              choices = c(",", ".")
                  ),
                  # Actualisation de la visualisation des données (10 premières lignes affichées)
                  actionButton("show", "Show first 10 rows")
                ),
                # 2ème box
                box(
                  title = "Data Preview",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  # Output correspondant à la visualisation des données du fichier uploadé
                  dataTableOutput("contents")
                )
              )
      ),
      
      # Statistics tab
      tabItem(tabName = "statistics",
              fluidRow(
                box(
                  title = "Descriptive Statistics",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 12,
                  # Visualisation des stats descriptives
                  verbatimTextOutput("statsOutput")
                )
              )
      ),
      
      # Train-test split tab
      tabItem(tabName = "traintest",
              fluidRow(
                # 1ère box
                box(
                  title = "Variable Selection",
                  status = "success",
                  solidHeader = TRUE,
                  width = 6,
                  # Choix multiple des variables prédictrices à partir du fichier de données
                  selectInput("predictors", "Select features:",
                              choices = names(data()),
                              multiple = TRUE
                  ),
                  # Choix unique de la variable cible à partir du fichier des données
                  selectInput("response", "Select label:",
                              choices = names(data()),
                              selected = names(data())
                  ),
                  # Actualisation des données pour le train-test split
                  actionButton("updateTrainTest", "Update Train/Test Data")
                ),
                # 2ème box
                box(
                  title = "Train Features Preview",
                  status = "success",
                  solidHeader = TRUE,
                  width = 6,
                  # Preview des variables prédictrices d'apprentissage
                  tableOutput("xTrainPreview")
                ),
                # 3ème box
                box(
                  title = "Class Distribution (priors)",
                  status = "success",
                  solidHeader = TRUE,
                  width = 6,
                  # Affichage du plot de distribution
                  plotOutput("classDistribution")
                )
              )
      ),
      tabItem(tabName = "model",
              fluidRow(
                # 1ère box
                box(
                  title = "Parameters of the Train Model",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  # Visualisation des paramètres du modèle
                  actionButton("trainModel", "Train your model"),
                  verbatimTextOutput("parameters")
                )),
              fluidRow(
                # 2ème box
                box(
                  title = "Probabilities of the trained model for each variable",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  # Visualisation des paramètres du modèle
                  verbatimTextOutput("probabilities")
                ),
                # 3ème box
                box(
                  title = "Model's Summary",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  verbatimTextOutput("summar")
                )
              )
      ),
      tabItem(tabName = "predictions",
              fluidRow(
                box(
                  title = "Posterior probabilities",
                  status = "success",
                  solidHeader = TRUE,
                  width = 6,
                  actionButton("predictButton", "Predict your data"),
                  verbatimTextOutput("posteriors")
                ),
                box(
                  title = "Predictions",
                  status = "success",
                  solidHeader = TRUE,
                  width = 6,
                  verbatimTextOutput("yPred")
                )
              )
              
      ),
      tabItem(tabName = "evaluation",
              fluidRow(
                box(
                  title = "Confusion Matrix",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 12,
                  actionButton("evaluate", "Evaluate your model"),
                  verbatimTextOutput("mc")
                ),
                box(
                  title = "Model's Performance",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 12,
                  verbatimTextOutput("performance")
                )
              )
      )
    )
  )
)


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
  y_pred <- reactiveVal(NULL)
  NB <- reactive({Gaussian_Naive_Bayes$new()})
  
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
  })
  
  
  
  observeEvent(input$trainModel, {
    print("input$trainModel")
    
    output$parameters <- renderPrint({
      return(fit <- NB()$fit(X_train(), y_train()))
    })
    
    output$probabilities <- renderPrint({
      return(print(NB()))
    })
    
    output$summar <- renderPrint({
      return(NB()$summary())
    })
  })
  
  observeEvent(input$predictButton, { #ici on avait inversé y_pred et y_proba dans l'affichage
    y_pred(NB()$predict(X_test(), threshold = 0.5, eps = 0))
    output$yPred <- renderPrint({
      return(y_pred())
    })
    
    y_proba = NB()$predict_proba(X_test())
    output$posteriors <- renderPrint({
      return(y_proba)
    })
    
    print(NB()$confusion_matrix(y_test(),y_pred()))
  })
  
  observeEvent(input$evaluate, {
    conf_mat <- NB()$confusion_matrix(y_test(),y_pred())
    output$mc <- renderPrint({
      return(conf_mat)
    })
    
    perf <- length(y_test()[y_pred() == y_test()]) / length(y_test())
    output$performance <- renderPrint({
      return(perf)
    })
  })
  
}

##### APP #####

shinyApp(ui, server)



