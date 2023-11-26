##' Gaussian_Naive_Bayes Class object
##' @importFrom R6 R6Class
##' @import parallel
##'
##' @export
Gaussian_Naive_Bayes <- R6Class("Gaussian_Naive_Bayes",
                                private = list(

                                  data = NULL,
                                  call = NULL,

                                  binarize = function(column) {
                                    if (is.numeric(column)) {
                                      return(as.matrix(column))
                                    } else if (is.factor(column) || is.character(column) || is.logical(column)) {
                                      return(model.matrix(~ column - 1))  # -1 to remove intercept term
                                    } else {
                                      message("Column with type ", class(column), " encountered.")
                                      stop("Only character, factor, logical, or numerical columns must be entered.")
                                    }
                                  },

                                  check_numeric = function (X) {
                                    if (!is.matrix(X)) {
                                      warning("x was coerced to a matrix.", call. = FALSE)
                                      X <- as.matrix(X)
                                    }

                                    if (!is.numeric(unlist(X))) {
                                      warning("Matrix elements were coerced to numeric")
                                      X <- as.matrix(as.numeric(unlist(X)))
                                    }
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
                                  #' @field X Training set.
                                  X  = NA,

                                  #' @field y Target variable to train the model with.
                                  y = NA,

                                  #' @field params Contains both mu et sd parameters.
                                  params = NA,

                                  #' @field prior Prior probabilities of each class in y.
                                  prior = NA,

                                  #' @field post Post probabilities of the classed data.
                                  post = NULL,

                                  #' @field vars Variables in X.
                                  vars = NULL,

                                  #' @field evels_y Classes in y.
                                  levels_y = NULL,

                                  #' @title fit
                                  #'
                                  #' @description Train the model.
                                  #'
                                  #' @param X a dataframe containing your training data set.
                                  #' @param y a vector of type factor, character or logical containing.
                                  fit = function(X,y) {
                                    #check if data is empty
                                    if (is.null(X))
                                      stop("predict.gaussian_naive_bayes(): X is required.", call. = FALSE)
                                    if (is.null(y))
                                      stop("predict.gaussian_naive_bayes(): y is required.", call. = FALSE)

                                    # check if the data is in vector :
                                    if (!is.factor(y) && !is.character(y) && !is.logical(y)) {
                                      stop("y must be either a factor, character, or logical vector", call. = FALSE)
                                    }
                                    #check if X is a dataframe
                                    if (!is.data.frame(X) && !is.matrix(X)) {
                                      stop("X must be either a factor, character, or logical vector", call. = FALSE)
                                    }

                                    if (any(is.na(X))) {
                                      stop("X cannot contain NA values.")
                                    }
                                    if (any(is.na(y))) {
                                      stop("y cannot contain NA values.")
                                    }

                                    self$X <- X
                                    self$y <- y

                                    # transform y into factor if it is not already :
                                    if (!is.factor(y)) {
                                      self$y <- factor(y)
                                    }

                                    levels_y <- levels(self$y) # get modalities of y

                                    nlev <- nlevels(self$y) # number of modalities of y

                                    # binarize X :
                                    num_cores <- detectCores()
                                    cl <- makeCluster(num_cores)
                                    self$X <- parLapply(cl, X, private$binarize)
                                    stopCluster(cl)

                                    self$X <- cbind(as.data.frame(self$X))

                                    self$X <- private$check_numeric(self$X)

                                    vars <- colnames(self$X)

                                    class_x <- class(X)[1] # matrix

                                    if (nlev < 2) {
                                      stop("y must contain at least two classes.", call. = FALSE)
                                    }

                                    y_counts <- stats::setNames(tabulate(self$y), levels_y)

                                    y_min <- y_counts <2

                                    if (any(y_min)) {
                                      stop("y variable has to contain at least two observation per class for the estimation process.", call. = FALSE)
                                    }

                                    prior <- NULL
                                    prior <- prop.table(y_counts)
                                    lev <- levels_y

                                    # calculate means et standard deviations for each predictor
                                    num_cores <- detectCores()
                                    cl <- makeCluster(num_cores)
                                    params <- do.call("rbind", parLapply(cl, levels_y, function(lev) {
                                      lev_subset <- self$X[self$y == lev, , drop = FALSE] #X by modalities of y

                                      if (all(sapply(lev_subset, is.numeric))) { #if lev_subset isn't numerical
                                        mu <- colMeans(lev_subset, na.rm = TRUE) # calculate mean
                                      } else { # if lev_subset isn't numerical
                                        mu <- apply(lev_subset, 2, function(x) {
                                          mean(x, na.rm = TRUE) # mean
                                        } )
                                      }

                                      sd <- apply(lev_subset, 2, function(x) { # calculate standard deviation
                                        sqrt(mean(x^2, na.rm = TRUE) - mean(x, na.rm = TRUE)^2)})

                                      rbind(mu, sd) # assemble mu et sd in rows
                                    } ))
                                    stopCluster(cl)

                                    # matrix of mu et sd values
                                    mu <- params[rownames(params) == "mu", ] # retrieve rows of mu
                                    rownames(mu) <- levels_y # affectate to mu corresponding modalities from y
                                    sd <- params[rownames(params) == "sd", ] # retrieve rows from sd
                                    rownames(sd) <- levels_y # affectate to sd corresponding modalities of y

                                    self$vars <- vars
                                    private$data <- list(x = self$X, y = self$y)
                                    self$levels_y <- levels_y
                                    self$params <- list(mu = mu, sd = sd)
                                    self$prior <- prior
                                    private$call <- match.call()
                                  },

                                  #' @title predict
                                  #'
                                  #' @description Use the model to predict test values
                                  #'
                                  #' @param X_test vector of character
                                  #' @param threshold numeric value
                                  #' @param eps numeric value
                                  #'
                                  #' @return vector of character
                                  #'
                                  #'
                                  predict = function(X_test,threshold = 0.001, eps = 0) {

                                    #check if X_test is empty
                                    if (is.null(X_test))
                                      stop("predict.gaussian_naive_bayes(): X_test is required.", call. = FALSE)

                                    #check if X_test is a dataframe
                                    if (!is.data.frame(X_test) && !is.matrix(X_test)) {
                                      stop("X_test must be either a factor, character, or logical vector", call. = FALSE)
                                    }

                                    # Check if there are any NA values in the input
                                    if (any(is.na(X_test))) {
                                      stop("X_test cannot contain NA values.")
                                    }

                                    num_cores <- detectCores()
                                    cl <- makeCluster(num_cores)
                                    X_test <- parLapply(cl, X_test, private$binarize)
                                    stopCluster(cl)

                                    X_test <- cbind(as.data.frame(X_test))

                                    X_test <- private$check_numeric(X_test)

                                    class_x <- class(X_test)[1]

                                    if (!is.matrix(X_test)){
                                      stop("predict.gaussian_naive_bayes(): X_test must be numeric matrix with at least one row and two named columns.", call. = FALSE)
                                    }
                                    if (!is.numeric(X_test)){
                                      stop("predict.gaussian_naive_bayes(): X_test must be a numeric matrix.", call. = FALSE)
                                    }

                                    lev <- self$levels_y
                                    prior <- self$prior
                                    mu <- self$params$mu
                                    sd <- self$params$sd
                                    col_names <- colnames(X_test)
                                    features <- col_names[col_names %in% colnames(mu)]

                                    mu <- mu[, features, drop = FALSE]
                                    sd <- sd[, features, drop = FALSE]
                                    n_features <- length(features)

                                    if (n_features == 0) {
                                      warning(paste0("predict.gaussian_naive_bayes(): no feature in X_test corresponds to ",
                                                     "features defined in the object. Classification is based on prior probabilities."), call. = FALSE)
                                    }

                                    sd[sd <= eps] <- threshold
                                    eps <- ifelse(eps == 0, log(.Machine$double.xmin), log(eps))
                                    threshold <- log(threshold)

                                    post <- matrix(nrow = nrow(X_test), ncol = length(lev))
                                    colnames(post) <- lev
                                    for (ith_class in seq_along(lev)) {
                                      ith_class_sd <- sd[ith_class, ]

                                      ith_post <- -0.5 * log(2 * pi * ith_class_sd^2) - 0.5 * ((X_test - mu[ith_class, ]) / ith_class_sd)^2

                                      ith_post[ith_post <= eps] <- threshold

                                      post[, ith_class] <- (rowSums(ith_post) + log(prior[ith_class]))
                                    }
                                    self$post <- post
                                    cat("\n Exponentiel \n")
                                    return(factor(lev[max.col(post, "first")], lev))
                                  },

                                  #' @title predict_proba
                                  #'
                                  #' @description Gives the post probabilities for each row of your test set.
                                  #'
                                  #'
                                  #' @param X_test Dataframe containing your test data
                                  #'
                                  #'
                                  predict_proba = function(X_test) {

                                    #check if X_test is empty
                                    if (is.null(X_test))
                                      stop("predict.gaussian_naive_bayes(): X_test is required.", call. = FALSE)

                                    #check if X_test is a dataframe
                                    if (!is.data.frame(X_test) && !is.matrix(X_test)) {
                                      stop("X_test must be either a factor, character, or logical vector", call. = FALSE)
                                    }

                                    # Check if there are any NA values in the input
                                    if (any(is.na(X_test))) {
                                      stop("X_test cannot contain NA values.")
                                    }

                                    n_obs <- nrow(X_test)
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

                                      colnames(result) <- self$levels_y
                                      return(result)
                                    }
                                  },

                                  #' @title confusion_matrix
                                  #'
                                  #' @description Creates a confusion matrix confronting the classed data to the real classes.
                                  #'
                                  #'
                                  #' @param y_test vector of characters containing test data
                                  #' @param y_pred vector of characters containing predictions
                                  #'
                                  #' @returns A matrix type object.
                                  #'
                                  confusion_matrix = function(y_test, y_pred){

                                    #check if data is empty
                                    if (is.null(y_test))
                                      stop("predict.gaussian_naive_bayes(): y_test is required.", call. = FALSE)
                                    if (is.null(y_pred))
                                      stop("predict.gaussian_naive_bayes(): y_pred is required.", call. = FALSE)

                                    # check if the data is in vector :
                                    if (!is.factor(y_test) && !is.character(y_test) && !is.logical(y_test)) {
                                      stop("y must be either a factor, character, or logical vector", call. = FALSE)
                                    }
                                    if (!is.factor(y_pred) && !is.character(y_pred) && !is.logical(y_pred)) {
                                      stop("y must be either a factor, character, or logical vector", call. = FALSE)
                                    }

                                    # Check if there are any NA values in the input
                                    if (any(is.na(y_test))) {
                                      stop("y_test cannot contain NA values.")
                                    }
                                    if (any(is.na(y_pred))) {
                                      stop("y_pred cannot contain NA values.")
                                    }

                                    cols = paste0('pred-', self$levels_y)
                                    conf_mat = array(dim = c(length(self$levels_y),length(self$levels_y)),dimnames = list(self$levels_y,cols))

                                    for (clas in self$levels_y) {
                                      for (clas_pred in cols) {
                                        compt <- table(y_pred[y_test == clas])
                                        conf_mat[clas,] <- compt
                                      }
                                    }
                                    return(conf_mat)
                                  },

                                  #' @title print
                                  #'
                                  #' @description Displays information data.
                                  #'

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

                                  #' @title summary
                                  #'
                                  #' @description displays information about the model and data.
                                  #'
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
