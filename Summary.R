

#summary avec les privates variables
#montre ce qu'il y a dans le modèle et ses paramètres
summary = function(){
  
  cat("- nobs: ",length(private$data$y))
  
  cat("- classes in y: ",private$levels_y,"\n") # names of classes in y

  #class_count number of training samples observed in each class in y.
  print(table(self$y))

  #class_prior_ probability of each class in y.
  cat("Class_prior_probabilities: ")
  print(prop.table(table(self$y)))
    
  #n_features_in_ Number of features seen during fit in x.
    cat("- n_Features:", length(private$vars), "\n")
    
  #feature_names_in_ Names of features seen during fit. Defined only when X has feature names that are all strings.
    cat("- Features:", private$vars, "\n")
    
  #standard deviation of each feature per class.
    cat("standard deviation of each feature")
    print(private$params$sd)

  #theta_ mean of each feature per class.
    cat("mean of each feature per class")
    print(private$param$mu)

}
