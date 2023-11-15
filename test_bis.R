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

y_train <- data_train[, "diagnosis"]
class(y_train) # vérification de la classe

X_test <- subset(data_test, select = -diagnosis)
dim(X_test) # vérification des dimensions du set

y_test <- data_train[, "diagnosis"]
class(y_test) # vérification de la classe

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
}

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
}

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

y <- y_train
X <- X_train

print(y)
class(y)
typeof(y)

print(X)
class(X)
typeof(X)

if (!is.factor(typeof(y)) && !is.character(typeof(y)) && !is.logical(typeof(y))) { # correction : ajout de typeof()
  stop("y must be either a factor, character, or logical vector", call. = FALSE)
}

class(y)

if (!is.factor(y)) {
  y <- factor(y)
}

class(y)

levels_y <- levels(y)
print(levels_y)
class(levels_y)

nlev <- nlevels(y)
print(nlev)

X <- lapply(X, binarize)
print(X)
class(X) # supp : liste
typeof(X)

X = cbind(as.data.frame(X))
class(X) # df
typeof(X)

vars <- colnames(X)
print(vars)
class(vars) # character

class_x <- class(X)[1]
class(class_x) # character
typeof(class_x)

class(X)
typeof(X)

is.numeric(X)

if (!is.matrix(X)) {
  print(class(X))
  print(typeof(X))
  warning("x was coerced to a matrix.", call. = FALSE)
  X <- as.matrix(X)
  print(class(X))
  print(typeof(X))
  if (!is.numeric(X)) { # correction : remplacement de mode(X) par typeof(X)
    X <- as.numeric(X)
    #stop("x must be a matrix/dgCMatrix with numeric columns.", call. = FALSE)
    print(class(X))
    print(typeof(X))
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
print(y_counts)

y_min <- y_counts <2
print(y_min)

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

print(prior)
lev <- levels_y

if (!NAx) {
  if (is.matrix(X)) {
    params <- do.call("rbind", lapply(levels_y, function(lev) {
      lev_subset <- X[y == lev, , drop = FALSE]
      
      print(class(lev_subset)) # à supprimer
      
      # Vérification si lev_subset est numérique
      if (all(sapply(lev_subset, is.numeric))) {
        mu <- colMeans(lev_subset, na.rm = TRUE)
      } else {
        mu <- apply(lev_subset, 2, function(X) mean(X, na.rm = TRUE))
      }
      
      print(mu) # à supprimer
      
      # Calcul de sd
      sd <- apply(lev_subset, 2, function(X) 
        sqrt(mean(X^2, na.rm = TRUE) - mean(X, na.rm = TRUE)^2))
      
      print(sd) # à supprimer
      
      rbind(mu, sd)
    }))
    
    mu <- params[rownames(params) == "mu", ]
    rownames(mu) <- levels_y
    sd <- params[rownames(params) == "sd", ]
    rownames(sd) <- levels_y
    
    print(mu) # à supprimer
    print(sd) # à supprimer
    
  } else { 
    mu <- ifelse(is.numeric(X) && !all(X %in% c(0,1)), 
                 rowsum(X, y, na.rm = TRUE) / y_counts, colMeans(X, na.rm=TRUE))
    sd <- ifelse(is.numeric(X) && !all(X %in% c(0,1)), 
                 sqrt((rowsum(X^2, y, na.rm = TRUE) - mu^2 * y_counts) / (y_counts - 1)), apply(X, 2, function(x) sqrt((sum(x) - sum(x^2)/length(x))/(length(x)-1))))
  }
  
  print(mu) # à supprimer
  print(sd) # à supprimer
  
} else {
  if (is.matrix(X)) { 
    na_per_feature <- lapply(levels_y, function(lev) {
      colSums(na_X[y == lev, , drop = FALSE], na.rm = TRUE)
    })
    
    print(na_per_feature) # à supprimer
    
    n_feature_obs <- y_counts - do.call("rbind", na_per_feature)
    rownames(n_feature_obs) <- levels_y
    n_feature_obs
  } else {
    y_counts - rowsum.default(na_X, y)
  }
  if (any(n < 2))
    warning("gaussian_naive_bayes(): infinite variances (NaN) are present, ",
            "in each case due to less than two observations after removing missing values.", call. = FALSE)
  
  if (is.matrix(X)) {
    params <- do.call("rbind", lapply(levels_y, function(lev) {
      lev_subset <- X[y == lev, , drop = FALSE]
      mu <- ifelse(all(sapply(lev_subset, is.numeric)), colMeans(lev_subset, na.rm = TRUE), apply(lev_subset, 2, function(x) mean(x, na.rm = TRUE)))
      nlev <- n[rownames(n) == lev]
      sd <- apply(lev_subset, 2, function(x) sqrt(mean(x^2, na.rm = TRUE) - mean(x, na.rm = TRUE)^2))
      rbind(mu, sd)
    }))
    mu <- params[rownames(params) == "mu", ]
    rownames(mu) <- levels_y
    sd <- params[rownames(params) == "sd", ]
    rownames(sd) <- levels_y
  } else {
    mu <- ifelse(is.numeric(X) && !all(X %in% c(0,1)), rowsum(X, y, na.rm = TRUE) / y_counts, colMeans(X, na.rm=TRUE))
    sd <- ifelse(is.numeric(X) && !all(X %in% c(0,1)), sqrt((rowsum(X^2, y, na.rm = TRUE) - mu^2 * y_counts) / (y_counts - 1)), apply(X, 2, function(x) sqrt((sum(x) - sum(x^2)/length(x))/(length(x)-1))))
  }
}













