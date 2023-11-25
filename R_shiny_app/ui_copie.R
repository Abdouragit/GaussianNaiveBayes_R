##### GAUSSIAN NAIVE BAYES FONCTION #####

if (!requireNamespace("R6", quietly = TRUE)) {
  install.packages("R6")
}
library(R6)

Gaussian_Naive_Bayes <- R6Class("Gaussian_Naive_Bayes",
                                private = list(
                                  
                                  #vars = NULL,
                                  data = NULL,
                                  #levels_y = NULL,
                                  #params = NULL,
                                  #prior = NULL,
                                  call = NULL,
                                  
                                  binarize = function(column) {
                                    if (is.numeric(column)) {
                                      return(model.matrix(~ column - 1))
                                    } else if (is.factor(column) || is.character(column) || is.logical(column)) {
                                      return(model.matrix(~ column - 1))  # -1 to remove intercept term
                                    } else {
                                      message("Column with type ", class(column), " encountered.")
                                      stop("Only character, factor, logical, or numerical columns must be entered.")
                                    }
                                  },
                                  
                                  check_numeric = function (X) {
                                    if (!is.matrix(X)) {
                                      #print(class(X))
                                      #print(typeof(X))
                                      warning("x was coerced to a matrix.", call. = FALSE)
                                      X <- as.matrix(X)
                                      #print(class(X))
                                      #print(typeof(X))
                                    }
                                    
                                    if (!is.numeric(unlist(X))) { 
                                      warning("Matrix elements were coerced to numeric")
                                      X <- as.matrix(as.numeric(unlist(X)))
                                    }
                                    
                                    #print(class(X)) # matrix, array
                                    #print(typeof(X)) # double
                                    return(X)
                                  },
                                  
                                  get_gaussian_tables = function() {
                                    if (!is.list(self$params))
                                      stop("get_gaussian_tables(): params must be a list with parameter estimates.", call. = FALSE)
                                    
                                    mu <- self$params$mu
                                    sd <- self$params$sd
                                    vars <- colnames(mu)
                                    n_tables <- ncol(mu)
                                    
                                    num_cores <- detectCores()
                                    cl <- makeCluster(num_cores)
                                    tables <- parLapply(cl, seq_len(n_tables), function(i) {
                                      ith_mu <- mu[, i]
                                      ith_sd <- sd[, i]
                                      
                                      ith_tab <- data.frame(mu = ith_mu, sd = ith_sd, row.names = names(self$prior))
                                      return (ith_tab)
                                    })
                                    stopCluster(cl)
                                    
                                    names(tables) <- vars
                                    class(tables) <- "naive_bayes_tables"
                                    attr(tables, "cond_dist") <- stats::setNames(rep("Gaussian", n_tables), vars)
                                    
                                    return(tables)
                                  }
                                ),
                                
                                public = list(
                                  #champs:
                                  X  = NA,
                                  y = NA,
                                  params = NA,
                                  prior = NA,
                                  post = NULL,
                                  vars = NULL,
                                  levels_y = NULL,
                                  
                                  fit = function(X,y, prior = NULL) {
                                    # vérifie que les données dans y sont bien un vecteur :
                                    if (!is.factor(typeof(y)) && !is.character(typeof(y)) && !is.logical(typeof(y))) {
                                      stop("y must be either a factor, character, or logical vector", call. = FALSE)
                                    }
                                    
                                    #class(y) # character
                                    
                                    # transforme y en facteur s'il n'est pas déjà un facteur :
                                    if (!is.factor(y)) {
                                      self$y <- factor(y)
                                    }
                                    
                                    #class(y) # factor
                                    
                                    levels_y <- levels(self$y) # récupère les modalités de y
                                    
                                    nlev <- nlevels(self$y) # nombre de modalités de y
                                    
                                    #class(X) # df
                                    #dim(X)
                                    
                                    # binarise X :
                                    num_cores <- detectCores()
                                    cl <- makeCluster(num_cores)
                                    self$X <- parLapply(cl, X, private$binarize)
                                    stopCluster(cl)
                                    
                                    #class(X) # list
                                    # no dim(X)
                                    
                                    self$X <- cbind(as.data.frame(self$X))
                                    
                                    #class(X) # df
                                    #dim(X)
                                    
                                    self$X <- private$check_numeric(self$X)
                                    
                                    #class(X) # matrix
                                    #dim(X)
                                    
                                    vars <- colnames(self$X)
                                    
                                    
                                    class_x <- class(X)[1] # matrix
                                    
                                    
                                    if (nlev < 2) {
                                      stop("y must contain at least two classes.", call. = FALSE)
                                    }
                                    
                                    if (is.null(vars)) { 
                                      stop("x must have unique column names.\n", call. = FALSE)
                                    }
                                    
                                    NAy <- anyNA(self$y) 
                                    NAx <- anyNA(self$X)
                                    
                                    if (NAy) { 
                                      na_y_bool <- is.na(self$y) 
                                      len_na <- sum(na_y_bool) 
                                      warning(paste0("y contains ", len_na, " missing",
                                                     ifelse(len_na == 1, " value", " values"), ". ",
                                                     ifelse(len_na == 1, "It is", "They are"),
                                                     " not included (also the corresponding rows in x) ",
                                                     "into the estimation process."), call. = FALSE)
                                      self$y <- self$y[!na_y_bool] 
                                      self$X <- self$X[!na_y_bool, ] 
                                    }
                                    
                                    if (NAx) { 
                                      na_X <- is.na(self$X) * 1 
                                      len_nax <- sum(na_X) 
                                      warning(paste0("x contains ", len_nax, " missing",
                                                     ifelse(len_nax == 1, " value", " values"), ". ",
                                                     "They are not included into the estimation process."),
                                              call. = FALSE)
                                    }
                                    
                                    y_counts <- stats::setNames(tabulate(self$y), levels_y)
                                    #print(y_counts)
                                    
                                    y_min <- y_counts <2
                                    #print(y_min)
                                    
                                    if (any(y_min)) {
                                      stop("y variable has to contain at least two observation per class for the estimation process.", call. = FALSE)
                                    }
                                    
                                    prior <- NULL
                                    
                                    if (is.null(prior)) { 
                                      prior <- prop.table(y_counts)
                                    } else {
                                      if (length(prior) != nlev) 
                                        stop(paste0("gaussian_naive_bayes(): Vector with prior probabilities should have ",
                                                    nlev, " entries"))
                                      prior <- stats::setNames(prior / sum(prior), levels_y)
                                    }
                                    
                                    lev <- levels_y
                                    
                                    # si pas de NaN dans X
                                    if (!NAx) {
                                      # calcul des moyennes et écarts-type pour chaque prédicteur dans chaque classe
                                      
                                      num_cores <- detectCores()
                                      cl <- makeCluster(num_cores)
                                      params <- do.call("rbind", parLapply(cl, levels_y, function(lev) {
                                        lev_subset <- self$X[self$y == lev, , drop = FALSE] # sous-matrice de X selon les modalités de y
                                        
                                        if (all(sapply(lev_subset, is.numeric))) { # si lev_subset numérique
                                          mu <- colMeans(lev_subset, na.rm = TRUE) # calcule moyenne
                                        } else { # si lev_subset pas numérique
                                          mu <- apply(lev_subset, 2, function(x) {print(class(x)) 
                                            mean(x, na.rm = TRUE) # moyenne
                                          } )
                                        }
                                        
                                        print(class(lev_subset))
                                        sd <- apply(lev_subset, 2, function(x) {print(class(x)) # calcule écart-type
                                          sqrt(mean(x^2, na.rm = TRUE) - mean(x, na.rm = TRUE)^2)})
                                        
                                        rbind(mu, sd) # rassemble les mu et sd en ligne
                                      }
                                      )
                                      )
                                      stopCluster(cl)
                                      
                                      # matrice des mu et sd
                                      mu <- params[rownames(params) == "mu", ] # retrieve les lignes "mu"
                                      rownames(mu) <- levels_y # affecte aux mu les modalités de y correspondantes
                                      sd <- params[rownames(params) == "sd", ] # # retrieve les lignes "sd"
                                      rownames(sd) <- levels_y # affecte aux sd les modalités de y correspondantes
                                      
                                      print(mu)
                                      print(sd)
                                      
                                      # si NaN dans X  
                                    } else {
                                      na_per_feature <- lapply(levels_y, function(lev) {
                                        colSums(na_X[self$y == lev, , drop = FALSE], na.rm = TRUE)
                                      })
                                      
                                      n_feature_obs <- y_counts - do.call("rbind", na_per_feature)
                                      rownames(n_feature_obs) <- levels_y
                                      n_feature_obs
                                      
                                      if (any(n < 2))
                                        warning("gaussian_naive_bayes(): infinite variances (NaN) are present, ",
                                                "in each case due to less than two observations after removing missing values.", call. = FALSE)
                                      
                                      params <- do.call("rbind", lapply(levels_y, function(lev) {
                                        lev_subset <- self$X[self$y == lev, , drop = FALSE]
                                        mu <- ifelse(all(sapply(lev_subset, is.numeric)), 
                                                     colMeans(lev_subset, na.rm = TRUE), 
                                                     apply(lev_subset, 2, 
                                                           function(x) mean(x, na.rm = TRUE)))
                                        nlev <- n[rownames(n) == lev]
                                        sd <- apply(lev_subset, 2, function(x) 
                                          sqrt(mean(x^2, na.rm = TRUE) - mean(x, na.rm = TRUE)^2))
                                        rbind(mu, sd)
                                      }))
                                      mu <- params[rownames(params) == "mu", ]
                                      rownames(mu) <- levels_y
                                      sd <- params[rownames(params) == "sd", ]
                                      rownames(sd) <- levels_y
                                    }
                                    self$vars <- vars # erreur
                                    private$data <- list(x = self$X, y = self$y)
                                    self$levels_y <- levels_y
                                    self$params <- list(mu = mu, sd = sd)
                                    self$prior <- prior
                                    private$call <- match.call()
                                  },
                                  
                                  predict = function(newdata,threshold = 0.001, eps = 0) {
                                    if (is.null(newdata))
                                      stop("predict.gaussian_naive_bayes(): newdata is required.", call. = FALSE)
                                    
                                    num_cores <- detectCores()
                                    cl <- makeCluster(num_cores)
                                    newdata <- parLapply(cl, newdata, private$binarize)
                                    stopCluster(cl)
                                    
                                    newdata <- cbind(as.data.frame(newdata))
                                    
                                    newdata <- private$check_numeric(newdata)
                                    
                                    class_x <- class(newdata)[1]
                                    
                                    
                                    
                                    if (!is.matrix(newdata)){
                                      stop("predict.gaussian_naive_bayes(): newdata must be numeric matrix with at least one row and two named columns.", call. = FALSE)
                                    }
                                    if (!is.numeric(newdata)){
                                      stop("predict.gaussian_naive_bayes(): newdata must be a numeric matrix.", call. = FALSE)
                                    }
                                    
                                    lev <- self$levels_y
                                    prior <- self$prior
                                    mu <- self$params$mu
                                    sd <- self$params$sd
                                    col_names <- colnames(newdata)
                                    features <- col_names[col_names %in% colnames(mu)]
                                    
                                    mu <- mu[, features, drop = FALSE]
                                    sd <- sd[, features, drop = FALSE]
                                    n_features <- length(features)
                                    
                                    if (n_features == 0) {
                                      warning(paste0("predict.gaussian_naive_bayes(): no feature in newdata corresponds to ",
                                                     "features defined in the object. Classification is based on prior probabilities."), call. = FALSE)
                                      #return(factor(rep(lev[which.max(prior)], nrow = newdata)), levels_y = lev)
                                    } 
                                    
                                    NAx <- anyNA(newdata)
                                    if (NAx) {
                                      ind_na <- which(is.na(newdata))
                                      len_na <- length(ind_na)
                                      warning("predict.gaussian_naive_bayes(): ", len_na, " missing",
                                              ifelse(len_na == 1, " value", " values"), " discovered in the newdata. ",
                                              ifelse(len_na == 1, "It is", "They are"), " not included in calculation.", call. = FALSE)
                                    }
                                    
                                    sd[sd <= eps] <- threshold
                                    eps <- ifelse(eps == 0, log(.Machine$double.xmin), log(eps))
                                    threshold <- log(threshold)
                                    
                                    
                                    
                                    post <- matrix(nrow = nrow(newdata), ncol = length(lev))
                                    colnames(post) <- lev
                                    for (ith_class in seq_along(lev)) {
                                      ith_class_sd <- sd[ith_class, ]
                                      
                                      ith_post <- -0.5 * log(2 * pi * ith_class_sd^2) - 0.5 * ((newdata - mu[ith_class, ]) / ith_class_sd)^2
                                      print(ith_post)
                                      
                                      if (NAx) ith_post[ind_na] <- 0
                                      ith_post[ith_post <= eps] <- threshold
                                      
                                      
                                      post[, ith_class] <- (rowSums(ith_post) + log(prior[ith_class]))
                                    } 
                                    self$post <- post
                                    print(post)
                                    cat("\n Exponentiel \n")
                                    print(exp(post))
                                    return(factor(lev[max.col(post, "first")], lev))
                                  },
                                  
                                  predict_proba = function(X) {
                                    n_obs <- nrow(X)
                                    n_lev <- length(self$levels_y)
                                    post <- matrix(0, nrow = n_obs, ncol = n_lev)
                                    
                                    
                                    if (n_obs == 1) { 
                                      post <- t(apply(post, 2, function(x) 1 / sum(exp(post - x))))
                                      colnames(post) <- self$levels_y
                                      return(post)
                                      
                                    } else {
                                      colnames(post) <- self$levels_y
                                      result <- matrix(0, nrow = n_obs, ncol = n_lev)
                                      
                                      for (i in seq_len(n_obs)) {
                                        
                                        probabilities <- exp(self$post[i,])
                                        sum_per_row <- sum(probabilities)
                                        
                                        for (j in seq_along(self$levels_y)) {
                                          result[i, j] <- probabilities[j] / sum_per_row
                                        }
                                      }
                                      
                                      
                                      if (n_obs ==1) {
                                        dimnames(result) <- list(NULL, self$levels_y)
                                        return(result)
                                      } else {
                                        return(result)
                                      }
                                    }
                                  }, 
                                  confusion_matrix = function(y_test, y_pred){
                                    cols = paste0('pred-', NB$levels_y)
                                    conf_mat = array(dim = c(length(self$levels_y),length(self$levels_y)),dimnames = list(self$levels_y,cols))
                                    
                                    for (clas in self$levels_y) {
                                      for (clas_pred in cols) {
                                        compt <- table(y_pred[y_test == clas])
                                        conf_mat[clas,] <- compt
                                      }
                                    }
                                    return(conf_mat)
                                  },
                                  
                                  print = function () {
                                    cat("\nPrior probabilities: \n")
                                    cat("----------------------------\n")
                                    for (lev in names(self$prior)) {
                                      cat(paste(" ", lev, ": ", self$prior[[lev]], "\n"))
                                    }
                                    
                                    cat("\nConditional Probabilities:\n")
                                    cat("----------------------------\n")
                                    tables <- private$get_gaussian_tables()
                                    for (var in names(tables)) {
                                      cat(paste("\n Variable:", var, "\n"))
                                      print(tables[[var]])
                                    }
                                  }, 
                                  
                                  summary = function(){
                                    cat(rep(" \n",2))
                                    cat(rep('=',10),'\n')
                                    cat("- Number of observations: ", length(self$y),"\n")
                                    cat(rep(" \n",2))
                                    
                                    #class_count number of training samples observed in each class in y.
                                    cat('- Number of training observation in each class y')
                                    print(table(self$y))
                                    cat(rep(" \n",2))
                                    
                                    #Prior_ probability of each class in y.
                                    cat("- Prior_probabilities in y: ")
                                    print(prop.table(table(self$y)))
                                    cat(rep(" \n",2))
                                    
                                    #n_features_in_ Number of features seen during fit.
                                    cat("- Number of Features:", length(self$vars), "\n")
                                    cat(rep(" \n",2))
                                    
                                    #feature_names_in_ Names of features seen during fit. Defined only when X has feature names that are all strings.
                                    cat("- Features:", self$vars, "\n")
                                    cat(rep(" \n",2))
                                    
                                    #standard deviation of each feature per class.
                                    cat("- Standard deviation of each feature: \n")
                                    print(self$params$sd)
                                    cat(rep(" \n",2))
                                    
                                    #theta_ mean of each feature per class.
                                    cat("- Mean of each feature per class: \n")
                                    print(self$params$mu)
                                    cat(rep('=',10),'\n')
                                    cat(rep(" \n",2))
                                  }
                                )
)

GNB <- Gaussian_Naive_Bayes$new()

##### SHINY APP #####

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

##### USER INTERFACE #####

ui <- dashboardPage(
  skin='purple',
  dashboardHeader(title = "Gaussian Naive Bayes Gaussian"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Import Data", tabName = "import", icon = icon("file-import")),
      menuItem("Statistics", tabName = "statistics", icon = icon("chart-bar")),
      menuItem("Train/Test", tabName = "traintest", icon = icon("sliders-h"))
    )
  ),
  
  dashboardBody(
    tabItems(
      #Home tab
      tabItem(tabName = "home",
              fluidRow(
                box(
                  title = "Instructions",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  "Welcome to the Shiny Dashboard! Select the 'Import Data' tab to upload and view data."
                )
              )
      ),
      
      # Import Data tab
      tabItem(tabName = "import",
              fluidRow(
                box(
                  title = "File Upload",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  selectInput("format", "Choose an option for file format:",
                              choices = c(".csv", ".xlsx")
                  ),
                  fileInput("file1", "Choose File",
                            accept = c(".csv", ".xlsx")
                  ),
                  selectInput("separator", "Choose separator character:",
                              choices = c(",", ";", "tab")
                  ),
                  selectInput("decimal", "Choose character for decimal points:",
                              choices = c(",", ".")
                  ), 
                  actionButton("show", "Show first 10 rows")
                ),
                box(
                  title = "Data Preview",
                  status = "success",
                  solidHeader = TRUE,
                  width = 12,
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
                  verbatimTextOutput("statsOutput")
                )
              )
      ),
      
      # Train-test split tab
      tabItem(tabName = "traintest",
              fluidRow(
                box(
                  title = "Variable Selection",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  selectInput("predictors", "Select predictor variables:",
                              choices = names(data()),
                              multiple = TRUE
                  ),
                  selectInput("response", "Select label:",
                              choices = names(data()),
                              selected = names(data())
                  ),
                  
                  actionButton("updateTrainTest", "Update Train/Test Data")
                ),
                box(
                  title = "XTrain Preview",
                  status = "success",
                  solidHeader = TRUE,
                  width = 6,
                  tableOutput("xTrainPreview")
                ),
                box(
                  title = "Class Distribution",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("classDistribution")
                )
              )
      )
    )
  )
)
