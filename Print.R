
                                # Method to print the model details
                                Print = function() {
                                  cat("Gaussian Naive Bayes Model:\n")
                                  cat("Training set:", self$target, "\n")
                                  cat("Number of features:", length(colnames(self$data)), "\n")
                                  # Add more information about the model if needed
                                },
                                


#surcharge de print
print.ACP <- function(object){
  #affichage de la liste des variables
  cat("Variables : ", colnames(object$X),"\n")
  #affichage des valeurs propres
  cat("Valeurs propres : ",object$vp)
}



#montre le la fonction et ses objets

#surcharge de print
print.GaussianNaiveBayes <- function(obj){
  #affichage de la liste des variables
  cat("Variables : ", colnames(object$X),"\n")
  
  
}


