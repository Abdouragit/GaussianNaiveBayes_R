if (!requireNamespace("shiny", quietly = TRUE)) {
  install.packages("shiny")
}
library(shiny)

if (!requireNamespace("DT", quietly = TRUE)) {
  install.packages("DT")
}
library(DT)

runExample('06_tabsets')
runExemple()

#if (!requireNamespace("shinythemes", quietly = TRUE)) {
  #install.packages("shinythemes")
#}
#library(shinythemes)

if (!requireNamespace("shinydashboard", quietly = TRUE)) {
  install.packages("shinydashboard")
}
library(shinydashboard)

#if (!requireNamespace("shinyjs", quietly = TRUE)) {
  #install.packages("shinyjs")
#}
#library(shinyjs)

#install.packages("Gaussian_Naive_Bayes")
#library(Gaussian_Naive_Bayes)

##### USER INTERFACE #####

ui <- dashboardPage(
  skin='purple',
  dashboardHeader(title = "Gaussian Naive Bayes Classifier"),
  dashboardSidebar(
    selectInput('filetype',
                'Choose a file type to display',
                choices=c('.csv', '.xlsx', '.txt')),
    fileInput('file1',
              'Choose a file',
              accept=c('.csv', '.xlsx', '.txt')),
    checkboxGroupInput("parameters",
                       "Model's parameters to display",
                       choices=c("Priors of each class",
                                 "Conditional probabilities",
                                 "The most probable class given the data")),
    selectInput('mc', 'Confusion Matrix',
                choices=c("Display",
                          "No display"))
  ),
  dashboardBody(
    fluidRow(
      tabsetPanel(type="tabs",
                  tabPanel('File Data', tableOutput('fileData')),
                  tabPanel('Parameters', verbatimTextOutput('params')),
                  tabPanel('Table'))
      #box(
        #title = "File Data",
        #tableOutput('fileData')
      #),
      #box(
        #title = "Parameters",
        #textOutput('params')
      )
    )
  )

##### SERVER #####

server <- function(input, output) {
  output$fileData <- renderTable({
    req(input$file1)  
    fileType <- tolower(input$filetype)
    if (fileType == '.csv') {
      data <- read.csv(input$file1$datapath)
    } else if (fileType == '.xlsx') {
      data <- read.xlsx(input$file1$datapath)
    } else if (fileType == '.txt') {
      data <- read.table(input$file1$datapath)
    }
    return(data)
  })
  
  output$params <- renderPrint({
    selectedParam <- input$parameters
    if (selectedParam == "Priors of each class") {
      return("Affichez les priors ici...")
    } else if (selectedParam == "Conditional probabilities") {
      return("Affichez les probabilités conditionnelles ici...")
    } else if (selectedParam == "The most probable class given the data") {
      return("Affichez la classe la plus probable ici...")
    } else {
      return("Sélectionnez un paramètre pour afficher les informations.")
    }
  })
  
  #observeEvent(input$mc, {
    #if (input$mc == "Display") {
      #confusionMatrix <- generateConfusionMatrix()
      #print(confusionMatrix)  # Replace this with your logic to display the confusion matrix
    #} else {
      #print("Confusion matrix will not be displayed.")
    #}
  #})
}

##### RESULT #####

shinyApp(ui, server)















