##### LIBRAIRIES UTILES #####

if (!requireNamespace("shiny", quietly = TRUE)) {
  install.packages("shiny")
}
library(shiny)

if (!requireNamespace("DT", quietly = TRUE)) {
  install.packages("DT")
}
library(DT)

#runExample('06_tabsets') # exemple de shiny

if (!requireNamespace("shinydashboard", quietly = TRUE)) {
  install.packages("shinydashboard")
}
library(shinydashboard)

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

if (!requireNamespace("caret", quietly = TRUE)) {
  install.packages("caret")
}
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
                  title = "Instructions",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  "Welcome to the Shiny Dashboard! Select the 'Data Uploading' tab to upload and view data."
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
                  width = 6,
                  # Visualisation des paramètres du modèle
                  actionButton("trainModel", "Train your model"),
                  verbatimTextOutput("parameters")
                ),
                # 2ème box
                box(
                  title = "Parameters of the Train Model",
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
                  width = 12,
                  verbatimTextOutput("summary")
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
