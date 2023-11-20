if (!requireNamespace("parallel", quietly = TRUE)) {
  install.packages("parallel")
}
library(parallel)

library(rsample)

data <- read.csv("/Users/annabellenarsama/Desktop/SISE/Programmation_R/données/dataset_long.csv")
print(data)

# train-test split :
set.seed(1) # reproducibilité !!!
split <- initial_split(data, prop = 0.7, strata = "sexe") # rsample : stratif° par label
data_train <- training(split)
data_test <- testing(split)

# split des prédicteurs (X) et du label (y) :
X_train <- subset(data_train, select = -sexe)
class(X_train) # df
dim(X_train) # (397, 30)

y_train <- data_train[, "sexe"]
class(y_train) # character (vecteur)

X_test <- subset(data_test, select = -sexe)
class(X_test) # df
dim(X_test) # (172, 30)

y_test <- data_test[, "sexe"]
class(y_test) # character (vecteur)

y <- y_train
X <- X_train

##### BINARIZE #####

binarize = function(column) {
  if (is.numeric(column)) {
    return(model.matrix(~ column - 1))
  } else if (is.factor(column) || is.character(column) || is.logical(column)) {
    return(model.matrix(~ column - 1))  # -1 to remove intercept term
  } else {
    message("Column with type ", class(column), " encountered.")
    stop("Only character, factor, logical, or numerical columns must be entered.")
  }
}

##### CHECK NUMERIC #####

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
}

##### GAUSSIAN TABLES #####

get_gaussian_tables = function() {
  if (!is.list(self$params))
    stop("get_gaussian_tables(): params must be a list with parameter estimates.", call. = FALSE)
  
  mu <- self$params$mu
  sd <- self$params$sd
  vars <- colnames(mu)
  n_tables <- ncol(mu)
  
  num_cores <- detectCores()
  cl <- makeCluster(num_cores)
  tables <- lapply(seq_len(n_tables), function(i) {
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

##### FIT #####

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

##### PREDICT #####

newdata <- X_test

if (is.null(newdata))
  stop("predict.gaussian_naive_bayes(): newdata is required.", call. = FALSE)

num_cores <- detectCores()
cl <- makeCluster(num_cores)
newdata <- parLapply(cl, newdata, private$binarize)
stopCluster(cl)





