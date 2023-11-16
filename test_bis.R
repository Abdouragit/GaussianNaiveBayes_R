# installation (si pas déjà fait) et chargement des librairies nécessaires :
# en dehors de la classe :
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

# split des prédicteurs (X) et du label (y) :
X_train <- subset(data_train, select = -diagnosis)
class(X_train) # df
dim(X_train) # (397, 30)

y_train <- data_train[, "diagnosis"]
class(y_train) # character (vecteur)

X_test <- subset(data_test, select = -diagnosis)
class(X_test) # df
dim(X_test) # (172, 30)

y_test <- data_train[, "diagnosis"]
class(y_test) # character (vecteur)

# fonction binarize :

binarize = function(column) {
  # vérifier que la colonne est numérique
  if (is.numeric(column)) {
    return(column)
  # sinon si colonne = vecteur factoriel, de caractères, ou logique
  } else if (is.factor(column) || is.character(column) || is.logical(column)) {
    # retrieve les valeurs uniques de colonnes
    unique_values <- unique(column)
    binary_columns <- lapply(unique_values, function(value) ifelse(column == value, 1, 0))
    names(binary_columns) <- paste0(names(column), "_", unique_values)
    return(as.data.frame(binary_columns))
  } else {
    message("Column with type ", class(column), " encountered.")
    stop("Only character, factor, or numerical columns must be entered.")
  }
}

# fonction check_numeric :

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

# fonction get_gaussian_tables :

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

# affectation des trainset aux objets de la classe :

y <- y_train
X <- X_train

print(y)
class(y) # vecteur factoriel, de caractères, ou logique
typeof(y) # character

print(X)
class(X) # df
typeof(X) # liste

# vérifie que les données dans y sont bien un vecteur :
if (!is.factor(typeof(y)) && !is.character(typeof(y)) && !is.logical(typeof(y))) {
  stop("y must be either a factor, character, or logical vector", call. = FALSE)
}

# vérifie la classe de y :
class(y) # character

# transforme y en facteur s'il n'est pas déjà un facteur :
if (!is.factor(y)) {
  y <- factor(y)
}

# vérifie la factorisation de y :
class(y) # factor

levels_y <- levels(y) # récupère les modalités de y
print(levels_y) # "B" "M"
class(levels_y) # character

nlev <- nlevels(y) # nombre de modalités de y
print(nlev) # 2

# binarise X :
X <- lapply(X, binarize)
# à supprimer après test :
print(X)
class(X) # liste
typeof(X) # liste

X = cbind(as.data.frame(X))
class(X) # df
typeof(X) # liste

vars <- colnames(X)
print(vars)
class(vars) # character

class_x <- class(X)[1]
class(class_x) # character
typeof(class_x) # character

is.numeric(X) # FALSE

if (!is.matrix(X)) {
  print(class(X)) # à supprimer après test
  print(typeof(X)) # à supprimer après test
  warning("x was coerced to a matrix.", call. = FALSE)
  X <- as.matrix(X)
  print(class(X)) # à supprimer après test
  print(typeof(X)) # à supprimer après test
  if (!is.numeric(X)) {
    X <- as.numeric(X)
    print(class(X)) # à supprimer après test
    print(typeof(X)) # à supprimer après test
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
print(y_counts) # B : 249 // M : 148

y_min <- y_counts <2
print(y_min) # B : FALSE // M : FALSE

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

# si pas de NaN dans X
if (!NAx) {
    # calcul des moyennes et écarts-type pour chaque prédicteur dans chaque classe
    params <- do.call("rbind", lapply(levels_y, function(lev) {
      lev_subset <- X[y == lev, , drop = FALSE] # sous-matrice de X selon les modalités de y
      
      print(class(lev_subset)) # à supprimer
      
      if (all(sapply(lev_subset, is.numeric))) { # si lev_subset numérique
        mu <- colMeans(lev_subset, na.rm = TRUE) # calcule moyenne
      } else { # si lev_subset pas numérique
        mu <- apply(lev_subset, 2, function(X) mean(X, na.rm = TRUE)) # calcule moyenne
      }
      
      print(mu) # à supprimer
      
      sd <- apply(lev_subset, 2, function(X) # calcule écart-type
        sqrt(mean(X^2, na.rm = TRUE) - mean(X, na.rm = TRUE)^2))
      
      print(sd) # à supprimer
      
      rbind(mu, sd) # rassemble les mu et sd en ligne
    }))
    
    # matrice des mu et sd
    mu <- params[rownames(params) == "mu", ] # retrieve les lignes "mu"
    rownames(mu) <- levels_y # affecte aux mu les modalités de y correspondantes
    sd <- params[rownames(params) == "sd", ] # # retrieve les lignes "sd"
    rownames(sd) <- levels_y # affecte aux sd les modalités de y correspondantes
    
    print(mu) # à supprimer
    print(sd) # à supprimer

# si NaN dans X  
} else { 
    na_per_feature <- lapply(levels_y, function(lev) {
      colSums(na_X[y == lev, , drop = FALSE], na.rm = TRUE)
    })
    
    print(na_per_feature) # à supprimer
    
    n_feature_obs <- y_counts - do.call("rbind", na_per_feature)
    rownames(n_feature_obs) <- levels_y
    n_feature_obs
  
  if (any(n < 2))
    warning("gaussian_naive_bayes(): infinite variances (NaN) are present, ",
            "in each case due to less than two observations after removing missing values.", call. = FALSE)
  
  params <- do.call("rbind", lapply(levels_y, function(lev) {
    lev_subset <- X[y == lev, , drop = FALSE]
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













