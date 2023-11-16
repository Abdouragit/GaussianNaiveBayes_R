rm(list = ls())


setwd("~/Desktop/M2SISE/Cours/Prog_Stats_R/ProjetRClasseR6")


data <- read.csv("Irismodifie.csv")

#data <- data[,-1]

#data$categorie <- rep(c("A", "B"), length.out =150)

#data <- data

#chemin_sortie_csv <- "~/Desktop/M2SISE/Cours/Prog_Stats_R/ProjetRClasseR6/Irismodifie.csv"

#write.csv(data, file = chemin_sortie_csv, row.names=FALSE)

library(R6)



set.seed(123)

# Créez un vecteur d'indices pour les données d'entraînement
indices_train <- sample(seq_len(nrow(data)), size = 0.8 * nrow(data))

# Créez les ensembles d'entraînement
Xtrain <- data[indices_train, !colnames(data) %in% "Species"]  # Exclut species
ytrain <- data[indices_train, "Species"]    # Dernière colonne

# Créez les ensembles de test
Xtest <- data[-indices_train, !colnames(data) %in% "Species"]  # Exclut la dernière colonne
ytest <- data[-indices_train, "Species"] 


Gaussian_Naive_Bayes <- R6Class("Gaussian_Naive_Bayes",
                                private = list(
                                  binarize = function(column) {
                                    if (is.numeric(column)) {
                                      return(as.matrix(column))
                                    } else if (is.factor(column) || is.character(column) || is.logical(column)) {
                                      unique_values <- unique(column)
                                      binary_columns <- lapply(unique_values, function(value) ifelse(column == value, 1, 0))
                                      names(binary_columns) <- paste0(names(column), "_", unique_values)
                                      return(as.matrix(binary_columns))
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
                                    
                                    #print(class(X))
                                    #print(typeof(X))
                                    return(X)
                                  },
                                  
                                  get_gaussian_tables = function(params) {
                                    if (!is.list(params))
                                      stop("get_gaussian_tables(): params must be a list with parameter estimates.", call. = FALSE)
                                    
                                    mu <- params$mu
                                    sd <- params$sd
                                    vars <- colnames(mu)
                                    n_tables <- ncol(mu)
                                    
                                    tables <- lapply(seq_len(n_tables), function(i) {
                                      ith_mu <- mu[, i]
                                      ith_sd <- sd[, i]
                                      
                                      ith_tab <- as.data.frame(cbind(ith_mu, ith_sd))
                                      colnames(ith_tab) <- c("mu", "sd")
                                      ith_tab
                                    })
                                    
                                    names(tables) <- vars
                                    class(tables) <- "naive_bayes_tables"
                                    attr(tables, "cond_dist") <- stats::setNames(rep("Gaussian", n_tables), vars)
                                    
                                    tables
                                  }
                                ),
                                
                                public = list(
                                  #champs:
                                  X  = NA,
                                  y = NA,
                                  params = NA,
                                  prior = NA,
                                  
                                  fit = function(X,y, prior = NULL) {
                                    # vérifie que les données dans y sont bien un vecteur :
                                    if (!is.factor(typeof(y)) && !is.character(typeof(y)) && !is.logical(typeof(y))) {
                                      stop("y must be either a factor, character, or logical vector", call. = FALSE)
                                    }

                                    # transforme y en facteur s'il n'est pas déjà un facteur :
                                    if (!is.factor(y)) {
                                      self$y <- factor(y)
                                    }
 
                                    levels_y <- levels(self$y) # récupère les modalités de y

                                    nlev <- nlevels(self$y) # nombre de modalités de y
                                    
                                    # binarise X :
                                    
                                    self$X <- lapply(X, private$binarize)
                                    
                                    self$X <- cbind(as.data.frame(self$X))
                                    
                                    self$X <- private$check_numeric(self$X)
                                    
                                    vars <- colnames(self$X)
                                    
                                    
                                    class_x <- class(self$X)[1]
                                    
                                    
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
                                    #print(y_counts) # B : 249 // M : 148
                                    
                                    y_min <- y_counts <2
                                    #print(y_min) # B : FALSE // M : FALSE
                                    
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
                                    
                                    #print(prior)
                                    
                                    lev <- levels_y
                                    
                                    # si pas de NaN dans X
                                    if (!NAx) {
                                      # calcul des moyennes et écarts-type pour chaque prédicteur dans chaque classe
                                      params <- do.call("rbind", lapply(levels_y, function(lev) {
                                        lev_subset <- self$X[self$y == lev, , drop = FALSE] # sous-matrice de X selon les modalités de y
                                        
                                        #print(class(lev_subset)) # à supprimer
                                        
                                        if (all(sapply(lev_subset, is.numeric))) { # si lev_subset numérique
                                          mu <- colMeans(lev_subset, na.rm = TRUE) # calcule moyenne
                                        } else { # si lev_subset pas numérique 
                                          print(class(mu))
                                          mu <- apply(lev_subset, 2, function(x) {print(class(x)) 
                                            mean(x, na.rm = TRUE)
                                          } ) # calcule moyenne
                                        }
                                        
                                        #print(mu) # à supprimer
                                        print(class(lev_subset))
                                        sd <- apply(lev_subset, 2, function(x) {print(class(x))# calcule écart-type
                                          sqrt(mean(x^2, na.rm = TRUE) - mean(x, na.rm = TRUE)^2)})
                                        
                                        #print(sd) # à supprimer
                                        
                                        rbind(mu, sd) # rassemble les mu et sd en ligne
                                      }))
                                      
                                      # matrice des mu et sd
                                      mu <- params[rownames(params) == "mu", ] # retrieve les lignes "mu"
                                      rownames(mu) <- levels_y # affecte aux mu les modalités de y correspondantes
                                      sd <- params[rownames(params) == "sd", ] # # retrieve les lignes "sd"
                                      rownames(sd) <- levels_y # affecte aux sd les modalités de y correspondantes
                                      
                                      #print(mu) # à supprimer
                                      #print(sd) # à supprimer
                                      
                                      # si NaN dans X  
                                    } else { 
                                      na_per_feature <- lapply(levels_y, function(lev) {
                                        colSums(na_X[self$y == lev, , drop = FALSE], na.rm = TRUE)
                                      })
                                      
                                      #print(na_per_feature) # à supprimer
                                      
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
                                    private$vars <- vars
                                    private$data <- list(x = self$X, y = self$y)
                                    private$levels_y <- levels_y
                                    private$params <- list(mu = mu, sd = sd)
                                    private$prior <- prior
                                    private$call <- match.call()
                                  },
                                  
                                  predict = function(newdata,threshold = 0.001, eps = 0) {
                                    if (is.null(newdata))
                                      stop("predict.gaussian_naive_bayes(): newdata is required.", call. = FALSE)
                                    
                                    newdata <- lapply(newdata, binarize)
                                    newdata <- cbind(as.data.frame(newdata))
                                    
                                    class_x <- class(newdata)[1]
                                    use_Matrix <- class_x == "dgCMatrix"
                                    
                                    if (!is.matrix(newdata) & !use_Matrix)
                                      stop("predict.gaussian_naive_bayes(): newdata must be numeric matrix or dgCMatrix (Matrix package) with at least one row and two named columns.", call. = FALSE)
                                    if (is.matrix(newdata) & mode(newdata) != "numeric")
                                      stop("predict.gaussian_naive_bayes(): newdata must be a numeric matrix.", call. = FALSE)
                                    if (use_Matrix & !"Matrix" %in% rownames(utils::installed.packages()))
                                      stop("predict.gaussian_naive_bayes(): please install Matrix package", call. = FALSE)
                                    
                                    lev <- private$levels_y
                                    prior <- private$prior
                                    mu <- private$params$mu
                                    sd <- private$params$sd
                                    row_names <- rownames(newdata)
                                    features <- row_names[row_names %in% colnames(mu)]
                                    mu <- mu[, features, drop = FALSE]
                                    sd <- sd[, features, drop = FALSE]
                                    n_features <- length(features)
                                    
                                    if (n_features == 0) {
                                      warning(paste0("predict.gaussian_naive_bayes(): no feature in newdata corresponds to ",
                                                     "features defined in the object. Classification is based on prior probabilities."), call. = FALSE)
                                      return(factor(rep(lev[which.max(prior)], nrow = newdata)), levels_y = lev)
                                    } 
                                    
                                    NAx <- anyNA(newdata)
                                    if (NAx) {
                                      ind_na <- if (use_Matrix) Matrix::which(is.na(newdata)) else which(is.na(newdata))
                                      len_na <- length(ind_na)
                                      warning("predict.gaussian_naive_bayes(): ", len_na, " missing",
                                              ifelse(len_na == 1, " value", " values"), " discovered in the newdata. ",
                                              ifelse(len_na == 1, "It is", "They are"), " not included in calculation.", call. = FALSE)
                                    }
                                    
                                    sd[sd <= eps] <- threshold
                                    eps <- ifelse(eps == 0, log(.Machine$double.xmin), log(eps))
                                    threshold <- log(threshold)
                                    
                                    post <- matrix(nrow = nrow(newdata), ncol = length(lev))
                                    for (ith_class in seq_along(lev)) {
                                      ith_class_sd <- sd[ith_class, ]
                                      ith_post <- -0.5 * log(2 * pi * ith_class_sd^2) - 0.5 * ((newdata - mu[ith_class, ]) / ith_class_sd)^2
                                      if (NAx) ith_post[ind_na] <- 0
                                      ith_post[ith_post <= eps] <- threshold
                                      post[, ith_class] <- if (use_Matrix) Matrix::colSums(ith_post) + log(prior[ith_class]) else colSums(ith_post) + log(prior[ith_class])
                                    }
                                    return(factor(lev[max.col(post, "first")], levels_y =lev))
                                  },
                                  
                                  predict_proba = function(X) {
                                    n_obs <- nrow(X)
                                    n_lev <- length(private$levels_y)
                                    post <- matrix(0, nrow = n_obs, ncol = n_lev)
                                    
                                    if (n_obs == 1) { 
                                      post <- t(apply(post, 2, function(x) 1 / sum(exp(post - x))))
                                      colnames(post) <- private$levels_y
                                      return(post)
                                      
                                    } else {
                                      colnames(post) <- private$levels_y
                                      result <- matrix(0, nrow = n_obs, ncol = n_lev)
                                      
                                      for (i in seq_len(n_obs)) {
                                        probabilities <- exp(post[i, ] - post[i, ])
                                        sum_per_row <- sum(probabilities)
                                        
                                        for (j in seq_along(private$levels_y)) {
                                          result[i, j] <- probabilities[j] / sum_per_row
                                        }
                                      }
                                      if (n_obs ==1) {
                                        dimnames(result) <- list(NULL, private$levels_y)
                                        return(result)
                                      } else {
                                        return(result)
                                      }
                                    }
                                  }, 
                                  
                                  print = function () {
                                    cat("Prior probabilities: \n")
                                    for (lev in names(private$prior)) {
                                      cat(paste(" ", lev, ": ", private$prior[[lev]], "\n"))
                                    }
                                    
                                    cat("\nConditional Probabilities:\n")
                                    tables <- get_gaussian_tables(private$params)
                                    for (var in names(tables)) {
                                      cat(paste("Variable:", var, "\n"))
                                      for (i in seq_along(tables[[var]])) {
                                        cat(paste(" Level", i, ":\n"))
                                        cat(tables[[var]][[i]], "\n\n")
                                      }
                                    }
                                  }, 
                                  
                                  summary = function(){
                                    
                                    cat("- Number of observations: ", length(private$data$y, "\n"))
                                    cat("- classes in y: ",private$levels_y,"\n") # names of classes in y
                                    
                                    #class_count number of training samples observed in each class in y.
                                    print(table(self$y))
                                    
                                    #class_prior_ probability of each class in y.
                                    cat("Class_prior_probabilities: ")
                                    print(prop.table(table(self$y)))
                                    
                                    #n_features_in_ Number of features seen during fit.
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
                                )
)


NB <- Gaussian_Naive_Bayes$new()
NB$fit(Xtrain, ytrain)


