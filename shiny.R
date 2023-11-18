install.packages('shiny')
library(shiny)

install.packages('DT')
library(DT)

#install.packages("Gaussian_Naive_Bayes")
#library(Gaussian_Naive_Bayes)

##### USER INTERFACE #####

library(shiny)

ui <- fluidPage(
  titlePanel("Gaussian Naive Bayes Classifier"),
  sidebarLayout(
    sidebarPanel(
      selectInput('filetype',
                  'Choose a file type to display',
                  choices=c('.csv', '.xlsx', '.txt')),
      fileInput('file1',
                'Choose a file',
                accept=c('.csv', '.xlsx', '.txt')),
      selectInput("parameters",
                  "Model's parameters to display",
                  choices=c("Priors of each class",
                            "Conditional probabilities",
                            "The most probable class given the data")),
      actionButton('mc', 'Display the confusion matrix')
    ),
    mainPanel(
      tableOutput('fileData'),
      selectOutput('params')
    )
  )
)

server <- function(input, output) {
  output$fileData <- renderTable({
    req(input$file1)  # on s'assure qu'un fichier a bien été téléchargé
    fileType <- tolower(input$filetype)
    
    # Ajouter la logique pour lire le fichier en fonction de son type
    if (fileType == '.csv') {
      # Lire un fichier CSV
      data <- read.csv(input$file1$datapath)
    } else if (fileType == '.xlsx') {
      # Lire un fichier Excel
      data <- read.xlsx(input$file1$datapath)
    } else if (fileType == '.txt') {
      # Lire un fichier texte
      data <- read.table(input$file1$datapath)
    }
    return(data)
  })
  observeEvent(input$mc, {
    # Logique à exécuter lorsque bouton cliqué
    # Ici code pour générer et afficher la matrice de confusion
    confusionMatrix <- generateConfusionMatrix()
    return(confusionMatrix)
  })
}

shinyApp(ui, server)














