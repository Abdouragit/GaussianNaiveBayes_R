library(R6)

Gaussian_Naive_Bayes <- R6Class("Gaussian_Naive_Bayes",
                                private = list(
                                  # fonction de binarisation des données catégorielles
                                  binarize = function(column) {
                                    # 'column' doit être numérique
                                    if (is.numeric(column)) {
                                      return(column)
                                    # si 'column' est un facteur ou une chaîne de caractères
                                    } else if (is.factor(column) || is.character(column)) {
                                      # 
                                      unique_values <- unique(column)
                                      # lapply : pas nécessaire de paralléliser ici
                                      binary_columns <- lapply(unique_values, function(value) ifelse(column == value, 1, 0))
                                      names(binary_columns) <- paste0(names(column), "_", unique_values)
                                      return(as.data.frame(binary_columns))
                                    } else {
                                      stop("Only character, factor, or numerical columns must be entered.")
                                    }
                                  }, 
                                  
                                  get_gaussian_tables = function(params) {
                                    # 'params' doit être une liste
                                    if (!is.list(params))
                                      # sinon message d'erreur
                                      stop("get_gaussian_tables(): params must be a list with parameter estimates.", call. = FALSE)
                                    
                                    # extraction des moyennes à partir de la liste 'params'
                                    mu <- params$mu
                                    # extraction des écarts-type à partir de la liste 'params'
                                    sd <- params$sd
                                    # extraction des noms des prédicteurs
                                    vars <- colnames(mu)
                                    # extraction du nombre des prédicteurs
                                    n_tables <- ncol(mu)
                                    
                                    # création de tables avec 2 colonnes (mu et sd) pour chaque i correspondant à la classe de y
                                    # lapply : pas nécessaire de paralléliser
                                    tables <- lapply(seq_len(n_tables), function(i) {
                                      ith_mu <- mu[, i]
                                      ith_sd <- sd[, i]
                                      # transformation des résultats en df
                                      ith_tab <- as.data.frame(cbind(ith_mu, ith_sd))
                                      # 
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
                                  X <- NA,
                                  y <- NA,
                                  params <- NA,
                                  prior <- NA,
                                  
                                  ### MÉTHODE FIT ###
                                  fit = function(X, y, prior = NULL) {
                                    if (!is.factor(y) && !is.character(y) && !is.logical(y)) {
                                      stop("y must be either a factor, character, or logical vector", call. = FALSE)
                                    }
                                    if (!is.factor(y)) {
                                      y <- factor(y)
                                    }
                                    levels <- levels(y)
                                    
                                    nlev <- nlevels(y)
                                    
                                    # lapply : parallélisation de binarize
                                    install.packages("parallel")
                                    library(parallel)
                                    # détection des coeurs de la machine
                                    num_cores <- detectCores()
                                    # création d'un cluster de coeurs
                                    cl <- makeCluster(num_cores)
                                    # export des objets nécessaires
                                    clusterExport(cl, c(self$X))
                                    # parallélisation
                                    self$X <- parLapply(cl, self$X, binarize)
                                    # arrêt du cluster
                                    stopCluster(cl)
                                    
                                    # combinaison des résultats
                                    self$X = cbind(as.data.frame(self$X))
                                    
                                    vars <- colnames(X) 
                                    class_x <- class(X)[1] 
                                    
                                    use_Matrix <- class_x %in% .matrix_classes 
                                    
                                    if (!is.matrix(X) && !use_Matrix) { 
                                      warning("X was coerced to a matrix.", call. = FALSE)
                                      x <- as.matrix(X) 
                                      if (mode(X) != "numeric") { 
                                        stop("X must be a matrix/dgCMatrix with integer columns.", call. = FALSE)
                                      }
                                    }
                                    
                                    if (use_Matrix) { 
                                      if (!"Matrix" %in% rownames(utils::installed.packages())) {
                                        stop("Please install the \"Matrix\" package.", call. = FALSE)
                                      }
                                      
                                      if (class_x != "dgCMatrix") {
                                        stop("dgCMatrix class from the Matrix package is the only supported class.", call. = FALSE)
                                      }
                                    }
                                    
                                    if (nlev < 2) {
                                      stop("y must contain at least two classes.", call. = FALSE)
                                    }
                                    
                                    if (is.null(vars)) { 
                                      stop("x must have unique column names.\n", call. = FALSE)
                                    }
                                    
                                    NAy <- anyNA(y) 
                                    NAx <- anyNA(X)
                                    
                                    if (NAy) { 
                                      na_y_bool <- is.na(y) 
                                      len_na <- sum(na_y_bool) 
                                      warning(paste0("y contains ", len_na, " missing",
                                                     ifelse(len_na == 1, " value", " values"), ". ",
                                                     ifelse(len_na == 1, "It is", "They are"),
                                                     " not included (also the corresponding rows in x) ",
                                                     "into the estimation process."), call. = FALSE)
                                      y <- y[!na_y_bool] 
                                      X <- X[!na_y_bool, ] 
                                    }
                                    
                                    if (NAx) { 
                                      na_X <- is.na(X) * 1 
                                      len_nax <- sum(na_X) 
                                      warning(paste0("x contains ", len_nax, " missing",
                                                     ifelse(len_nax == 1, " value", " values"), ". ",
                                                     "They are not included into the estimation process."),
                                              call. = FALSE)
                                    }
                                    
                                    y_counts <- stats::setNames(tabulate(y), levels) 
                                    y_min <- y_counts <2 
                                    
                                    if (any(y_min)) {
                                      stop("y variable has to contain at least two observation per class for the estimation process.", call. = FALSE)
                                    }
                                    
                                    if (is.null(prior)) { 
                                      prior <- prop.table(y_counts)
                                    } else {
                                      if (length(prior) != nlev) 
                                        stop(paste0("gaussian_naive_bayes(): Vector with prior probabilities should have ",
                                                    nlev, " entries"))
                                      prior <- stats::setNames(prior / sum(prior), levels)
                                    }
                                    # si pas de données manquantes dans X
                                    if (!NAx) { 
                                      # installation du package "data.table" si pas déjà installé
                                      if (!requireNamespace("data.table", quietly = TRUE)) {
                                        install.packages("data.table")
                                      }
                                      # si 'use_Matrix' vraie
                                      if (use_Matrix) {
                                        # chargement de la librairie 'parallel'
                                        library(parallel)
                                        # chargement de la librairie 'data.table'
                                        library(data.table)
                                        # détection du nombre de coeurs de la machine
                                        num_cores <- detectCores()
                                        # création d'un cluster
                                        cl <- makeCluster(num_cores)
                                        # export des objets nécessaires vers le cluster
                                        clusterExport(cl, "X", "y", "levels", "y_counts")
                                        # parallélisation avec parLapply
                                        params_list <- parLapply(cl, levels, function(lev) {
                                          # sous-matrices des X en fonction des modalités de y
                                          lev_subset <- X[y == lev, , drop = FALSE]
                                          # moyennes de chaque prédicteur dans chaque classe
                                          mu <- ifelse(is.numeric(lev_subset) && !all(lev_subset %in% c(0, 1)), Matrix::colMeans(lev_subset, na.rm = TRUE), colMeans(lev_subset, na.rm = TRUE))
                                          # écarts-type de chaque prédicteur dans chaque classe
                                          sd <- ifelse(is.numeric(lev_subset) && !all(lev_subset %in% c(0, 1)),
                                                       sqrt((Matrix::colSums(lev_subset^2, na.rm = TRUE) - mu^2 * y_counts[lev]) / (y_counts[lev] - 1)),
                                                       apply(lev_subset, 2, function(x) sqrt((sum(x) - sum(x^2) / length(x)) / (length(x) - 1))))
                                          # rbindlist from data.table
                                          rbindlist(list(mu = mu, sd = sd), fill = TRUE)
                                        })
                                        # arrêt du cluster
                                        stopCluster(cl)
                                        # combinaison des résultats
                                        params <- do.call(rbind, params_list)
                                        # extraction des mu et sd
                                        mu <- params[rownames(params) == "mu", ]
                                        rownames(mu) <- levels
                                        sd <- params[rownames(params) == "sd", ]
                                        rownames(sd) <- levels
                                      # si 'use_Matrix' fausse
                                      } else { 
                                        mu <- ifelse(is.numeric(X) && !all(X %in% c(0,1)), rowsum(X, y, na.rm = TRUE) / y_counts, colMeans(X, na.rm=TRUE))
                                        sd <- ifelse(is.numeric(X) && !all(X %in% c(0,1)), sqrt((rowsum(X^2, y, na.rm = TRUE) - mu^2 * y_counts) / (y_counts - 1)), apply(X, 2, function(x) sqrt((sum(x) - sum(x^2)/length(x))/(length(x)-1))))
                                      }
                                    # si données manquantes dans X  
                                    } else {
                                      # si 'use_Matrix' vraie
                                      n <- if (use_Matrix) { 
                                        # lapply : parallélisation
                                        library(parallel)
                                        num_cores <- detectCores()
                                        cl <- makeCluster(num_cores)
                                        # export des objets nécessaires vers le cluster
                                        clusterExport(cl, "na_X", "y", "levels", "y_counts")
                                        # parallélisation avec parLapply
                                        na_per_feature <- parLapply(cl, levels, function(lev) {
                                          Matrix::colSums(na_X[y == lev, , drop = FALSE], na.rm = TRUE)
                                        })
                                        # arrêt du cluster
                                        stopCluster(cl)
                                        # combinaison des résultats
                                        n_feature_obs <- y_counts - do.call("rbind", na_per_feature)
                                        rownames(n_feature_obs) <- levels
                                        n_feature_obs
                                      } else {
                                        y_counts - rowsum.default(na_X, y)
                                      }
                                      # si valeurs valides < 2
                                      if (any(n < 2))
                                        # alerte générée
                                        warning("gaussian_naive_bayes(): infinite variances (NaN) are present, ",
                                                "in each case due to less than two observations after removing missing values.", call. = FALSE)
                                      # si 'use_Matrix' vraie
                                      if (use_Matrix) {
                                        # lapply : parallélisation
                                        library(parallel)
                                        library(data.table)
                                        # détection du nombre de coeurs
                                        num_cores <- detectCores()
                                        # création du cluster
                                        cl <- makeCluster(num_cores)
                                        # export des objets nécessaires vers le cluster
                                        clusterExport(cl, c("X", "y", "levels", "n", "y_counts"))
                                        # parallélisation avec parLapply
                                        params_list <- parLapply(cl, levels, function(lev) {
                                          lev_subset <- X[y == lev, , drop = FALSE]
                                          mu <- ifelse(is.numeric(lev_subset) && !all(lev_subset %in% c(0, 1)), Matrix::colMeans(lev_subset, na.rm = TRUE), colMeans(lev_subset, na.rm = TRUE))
                                          nlev <- n[rownames(n) == lev]
                                          sd <- ifelse(is.numeric(lev_subset) && !all(lev_subset %in% c(0, 1)),
                                                       sqrt((Matrix::colSums(lev_subset^2, na.rm = TRUE) - mu^2 * y_counts[lev]) / (y_counts[lev] - 1)),
                                                       apply(lev_subset, 2, function(x) sqrt((sum(x) - sum(x^2) / length(x)) / (length(x) - 1))))
                                          rbindlist(list(mu = mu, sd = sd), fill = TRUE)
                                        })
                                        # arrêt du cluster parallèle
                                        stopCluster(cl)
                                        # combinaison des résultats avec rbindlist
                                        params <- rbindlist(params_list)
                                        # extraction des mu et sd
                                        mu <- params[rownames(params) == "mu", ]
                                        rownames(mu) <- levels
                                        sd <- params[rownames(params) == "sd", ]
                                        rownames(sd) <- levels
                                      } else {
                                        mu <- ifelse(is.numeric(X) && !all(X %in% c(0,1)), rowsum(X, y, na.rm = TRUE) / y_counts, colMeans(X, na.rm=TRUE))
                                        sd <- ifelse(is.numeric(X) && !all(X %in% c(0,1)), sqrt((rowsum(X^2, y, na.rm = TRUE) - mu^2 * y_counts) / (y_counts - 1)), apply(X, 2, function(x) sqrt((sum(x) - sum(x^2)/length(x))/(length(x)-1))))
                                      }
                                    }
                                    
                                    private$data <- list(x = X, y = y)
                                    private$levels <- levels
                                    private$params <- list(mu = mu, sd = sd)
                                    private$prior <- prior
                                    private$call <- match.call()
                                  }, # FIN DE FIT
                                  
                                  ### MÉTHODE PREDICT ###
                                  predict = function(newdata, threshold = 0.001, eps = 0) {
                                    if (is.null(newdata))
                                      stop("predict.gaussian_naive_bayes(): newdata is required.", call. = FALSE)
                                    
                                    # lapply : parallélisation
                                    library(parallel)
                                    num_cores <- detectCores()
                                    cl <- makeCluster(num_cores)
                                    clusterExport(cl, "binarize")
                                    newdata <- parLapply(cl, newdata, binarize)
                                    stopCluster(cl)
                                    
                                    newdata <- cbind(as.data.frame(newdata))
                                    
                                    # vérification de la classe pour utiliser Matrix
                                    use_Matrix <- any(sapply(newdata, function(col) class(col)[1] == "dgCMatrix"))
                                    
                                    if (!is.matrix(newdata) & !use_Matrix)
                                      stop("predict.gaussian_naive_bayes(): newdata must be numeric matrix or dgCMatrix (Matrix package) with at least one row and two named columns.", call. = FALSE)
                                    if (is.matrix(newdata) & mode(newdata) != "numeric")
                                      stop("predict.gaussian_naive_bayes(): newdata must be a numeric matrix.", call. = FALSE)
                                    if (use_Matrix & !"Matrix" %in% rownames(utils::installed.packages()))
                                      stop("predict.gaussian_naive_bayes(): please install Matrix package", call. = FALSE)
                                    
                                    lev <- private$levels
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
                                      return(factor(rep(lev[which.max(prior)], nrow = newdata)), levels = lev)
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
                                    return(factor(lev[max.col(post, "first")], levels =lev))
                                  },
                                  
                                  ### MÉTHODE PREDICT.PROBA ###
                                  predict_proba = function(X) {
                                    n_obs <- nrow(X)
                                    n_lev <- length(private$levels)
                                    post <- matrix(0, nrow = n_obs, ncol = n_lev)
                                    
                                    if (n_obs == 1) { 
                                      post <- t(apply(post, 2, function(x) 1 / sum(exp(post - x))))
                                      colnames(post) <- private$levels
                                      return(post)
                                      
                                    } else {
                                      colnames(post) <- private$levels
                                      result <- matrix(0, nrow = n_obs, ncol = n_lev)
                                      
                                      for (i in seq_len(n_obs)) {
                                        probabilities <- exp(post[i, ] - post[i, ])
                                        sum_per_row <- sum(probabilities)
                                        
                                        for (j in seq_along(private$levels)) {
                                          result[i, j] <- probabilities[j] / sum_per_row
                                        }
                                      }
                                      if (n_obs ==1) {
                                        dimnames(result) <- list(NULL, private$levels)
                                        return(result)
                                      } else {
                                        return(result)
                                      }
                                    }
                                  }, 
                                  
                                  ### MÉTHODE PRINT ###
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
                                  
                                  ### MÉTHODE SUMMARY ###
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
                      
                                  