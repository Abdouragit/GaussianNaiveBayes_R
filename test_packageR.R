# installation et chargement des librairies nécessaires :
if (!requireNamespace("xlsx", quietly = TRUE)) {
  install.packages("xlsx")
}
library(xlsx)
if (!requireNamespace("rsample", quietly = TRUE)) {
  install.packages("rsample")
}
library(rsample)
if (!requireNamespace("Matrix", quietly = TRUE)) {
  install.packages("Matrix")
}
library(Matrix)

library(R6)

# chargement du dataset :
data <- read.csv("/Users/annabellenarsama/Desktop/SISE/Programmation_R/breast-cancer.csv")
print(data)

# inspection du dataset :
head(data) # premières données
class(data) # type d'objet du dataset
summary(data) # résumé des données
colnames(data) # noms des prédicteurs
dim(data) # dimension du dataset

# drop des variables inutiles :
data <- subset(data, select = -id)
dim(data) # vérification des dimensions

# proportion des classes de y :
prop_y = table(data$diagnosis)/sum(table(data$diagnosis))
print(prop_y)

# train-test split :
set.seed(1) # reproducibilité !!!
split <- initial_split(data, prop = 0.7, strata = "diagnosis") # rsample : stratif° par label
data_train <- training(split)
data_test <- testing(split)
# dimensions de chaque set :
dim(data_train)
dim(data_test)
# vérification de la stratification par le label :
table(data_train$diagnosis)/sum(table(data_train$diagnosis))
table(data_test$diagnosis)/sum(table(data_test$diagnosis))

# split des prédicteurs (X) et de la cible (y) :
X_train <- subset(data_train, select = -diagnosis)
dim(X_train) # vérification des dimensions du set

y_train <- subset(data_train, select = diagnosis)
dim(y_train) # vérification des dimensions du set

X_test <- subset(data_test, select = -diagnosis)
dim(X_test) # vérification des dimensions du set

y_test <- subset(data_test, select = diagnosis)
dim(y_test) # vérification des dimensions du set

# conversion de X et y (df) en matrices :
X_train <- as.matrix(X_train)
class(X_train)

X_test <- as.matrix(X_test)
class(X_test)

y_train <- as.matrix(y_train)
class(y_train)

y_test <- as.matrix(y_test)
class(y_test)

Gaussian_Naive_Bayes <- R6Class("Gaussian_Naive_Bayes",
                                private = list(
                                  binarize = function(column) {
                                    if (is.numeric(column)) {
                                      return(column)
                                    } else if (is.factor(column) || is.character(column) || is.logical(column)) {
                                      unique_values <- unique(column)
                                      binary_columns <- lapply(unique_values, function(value) ifelse(column == value, 1, 0))
                                      names(binary_columns) <- paste0(names(column), "_", unique_values)
                                      return(as.data.frame(binary_columns))
                                    } else {
                                      message("Column with type ", class(column), " encountered.")
                                      stop("Only character, factor, or numerical columns must be entered.")
                                    }
                                  },
                                  
                                  check_numeric = function(X) {
                                    # Assurez-vous que les noms de colonnes sont uniques
                                    colnames(X) <- make.names(colnames(X), unique = TRUE)
                                    
                                    if (!is.matrix(X)) {
                                      warning("X was coerced to a matrix.", call. = FALSE)
                                      X <- as.matrix(as.data.frame(X))
                                    }
                                    
                                    non_numeric_columns <- lapply(X, function(col) !is.numeric(col))
                                    
                                    if (any(unlist(non_numeric_columns))) {
                                      warning("Columns ", paste(names(non_numeric_columns)[unlist(non_numeric_columns)], collapse = ", "), " were coerced to numeric(columns ont été converties en numérique).", call. = FALSE)
                                      X[, names(non_numeric_columns)[unlist(non_numeric_columns)]] <- lapply(X[, names(non_numeric_columns)[unlist(non_numeric_columns)]], as.numeric)
                                    }
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
                                  # champs :
                                  X  = NA,
                                  y = NA,
                                  params = NA,
                                  prior = NA,
                                  
                                  fit = function(X, y, prior = NULL) {
                                    if (!is.factor(typeof(y)) && !is.character(typeof(y)) && !is.logical(typeof(y))) { # correction : ajout de typeof()
                                      stop("y must be either a factor, character, or logical vector", call. = FALSE)
                                    }
                                    if (!is.factor(typeof(y))) {
                                      y <- factor(y)
                                    }
                                    
                                    levels_y <- levels(y)
                                    
                                    nlev <- nlevels(y)
                                    
                                    self$X <- lapply(self$X, private$binarize)
                                    
                                    print("binarization en cours dans le fit")
                                    self$X = cbind(as.data.frame(self$X))
                                    
                                    print("binarize fit est réussi")
                                    
                                    print("vars en cours")
                                    vars <- colnames(X)
                                    print("vars ok")
                                    
                                    print("class_x en cours")
                                    class_x <- class(X)[1]
                                    print("class_x ok")
                                    
                                    print("use_Matrix en cours")
                                    use_Matrix <- class_x %in% .matrix_classes # correction : erreur
                                    print("use_Matrix ok")
                                    
                                    if (!is.matrix(X) && !use_Matrix) { 
                                      warning("x was coerced to a matrix(x a été converti en matrice).", call. = FALSE)
                                      x <- as.matrix(X) 
                                      if (typeof(X) != "numeric") { # correction : remplacement de mode(X) par typeof(X)
                                        stop("x must be a matrix/dgCMatrix with integer columns.", call. = FALSE)
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
                                    
                                    y_counts <- stats::setNames(tabulate(y), levels_y) 
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
                                      prior <- stats::setNames(prior / sum(prior), levels_y)
                                    }
                                    
                                    if (!NAx) { 
                                      if (use_Matrix) { 
                                        params <- do.call("rbind", lapply(levels_y, function(lev) {
                                          lev_subset <- X[y == lev, , drop = FALSE]
                                          mu <- ifelse(is.numeric(lev_subset) && !all(lev_subset %in% c(0, 1)),Matrix::colMeans(lev_subset, na.rm = TRUE),colMeans(lev_subset, na.rm = TRUE))
                                          sd <- ifelse(is.numeric(lev_subset) && !all(lev_subset %in% c(0, 1)),sqrt((Matrix::colSums(lev_subset^2, na.rm = TRUE) - mu^2 * y_counts[lev]) / (y_counts[lev] - 1)),apply(lev_subset, 2, function(x) sqrt((sum(x) - sum(x^2) / length(x)) / (length(x) - 1))))
                                          rbind(mu, sd) }))
                                        mu <- params[rownames(params) == "mu", ]
                                        rownames(mu) <- levels_y
                                        sd <- params[rownames(params) == "sd", ]
                                        rownames(sd) <- levels_y
                                      } else { 
                                        mu <- ifelse(is.numeric(X) && !all(X %in% c(0,1)), rowsum(X, y, na.rm = TRUE) / y_counts, colMeans(X, na.rm=TRUE))
                                        sd <- ifelse(is.numeric(X) && !all(X %in% c(0,1)), sqrt((rowsum(X^2, y, na.rm = TRUE) - mu^2 * y_counts) / (y_counts - 1)), apply(X, 2, function(x) sqrt((sum(x) - sum(x^2)/length(x))/(length(x)-1))))
                                      }
                                      
                                    } else { 
                                      n <- if (use_Matrix) { 
                                        na_per_feature <- lapply(levels_y, function(lev) {
                                          Matrix::colSums(na_X[y == lev, , drop = FALSE], na.rm = TRUE)
                                        })
                                        n_feature_obs <- y_counts - do.call("rbind", na_per_feature)
                                        rownames(n_feature_obs) <- levels_y
                                        n_feature_obs
                                      } else {
                                        y_counts - rowsum.default(na_X, y)
                                      }
                                      if (any(n < 2))
                                        warning("gaussian_naive_bayes(): infinite variances (NaN) are present, ",
                                                "in each case due to less than two observations after removing missing values.", call. = FALSE)
                                      
                                      if (use_Matrix) {
                                        params <- do.call("rbind", lapply(levels_y, function(lev) {
                                          lev_subset <- X[y == lev, , drop = FALSE]
                                          mu <- ifelse(is.numeric(lev_subset) && !all(lev_subset %in% c(0, 1)),Matrix::colMeans(lev_subset, na.rm = TRUE),colMeans(lev_subset, na.rm = TRUE))
                                          nlev <- n[rownames(n) == lev]
                                          sd <- ifelse(is.numeric(lev_subset) && !all(lev_subset %in% c(0, 1)),sqrt((Matrix::colSums(lev_subset^2, na.rm = TRUE) - mu^2 * y_counts[lev]) / (y_counts[lev] - 1)),apply(lev_subset, 2, function(x) sqrt((sum(x) - sum(x^2) / length(x)) / (length(x) - 1))))
                                          rbind(mu, sd) }))
                                        mu <- params[rownames(params) == "mu", ]
                                        rownames(mu) <- levels_y
                                        sd <- params[rownames(params) == "sd", ]
                                        rownames(sd) <- levels_y
                                      } else {
                                        mu <- ifelse(is.numeric(X) && !all(X %in% c(0,1)), rowsum(X, y, na.rm = TRUE) / y_counts, colMeans(X, na.rm=TRUE))
                                        sd <- ifelse(is.numeric(X) && !all(X %in% c(0,1)), sqrt((rowsum(X^2, y, na.rm = TRUE) - mu^2 * y_counts) / (y_counts - 1)), apply(X, 2, function(x) sqrt((sum(x) - sum(x^2)/length(x))/(length(x)-1))))
                                      }
                                    }
                                    private$vars <- vars
                                    private$data <- list(x = X, y = y)
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
                                    if (is.matrix(newdata) & typeof(newdata) != "numeric") # corr : remplacement de mode(newdata) par typeof(newdata)
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
NB$fit(X_train, y_train)




   