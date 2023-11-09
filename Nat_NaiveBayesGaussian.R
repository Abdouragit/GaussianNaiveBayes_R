library(R6)

Gaussian_Naive_Bayes = R6Class("Gaussian_Naive_Bayes",
                                #membres publics: comprennent les méthodes 'initialize', 'fit', 'predict', 'predict_proba'.
                                public = list(     
                                  #constructeur
                                  initialize = function() {
                                    private$data <- NULL
                                    private$levels <- NULL
                                    private$params <- NULL
                                    private$prior <- NULL
                                    private$call <- NULL
                                  },
                                  # Ajouter une fonction qui permet de transformer les données: conserver les variables quantitatives numeric et integer.
                                  
                                  
                                  
                                  # Fonction qui permet la standardisation des données si les données n'ont pas les mêmes unités (cela deviendra un paramètre que l'utilisateur pourra activer ou non):
                                  
                                  centrage_reduction = function(X) { # X:dataframe qu'on veut centrer et réduire
                                    instance <- list() #instance est un objet crée pour stocker les résultats de la fonction
                                    archive.ect_mean <- function(x) {
                                      n<-length(x)
                                      moy<-mean(x)
                                      ect <- sqrt((n-1)/n*var(x)) # var provient du package stat? Coder ce var 
                                      return(c(moy,ect))
                                    }
                                    one.cr  = function(x) {
                                      n<-length(x)
                                      moy<-mean(x)
                                      ect <- sqrt((n-1)/n*var(x))
                                      y<- (x-moy)/ect # on centre et réduit les valeurs les valeurs du vecteur x. 
                                      return(y) # y est le vecteur centré et réduit de x
                                    }
                                    Y <- as.data.frame(lapply(X, one.cr))# on applique la fonction one.cr à chaque colonne du dataframe X, cela crée un dataframe Y où chaque colonne est centrée et réduite. 
                                    Arch <- as.data.frame(lapply(X, archive.ect_mean)) #cela crée un nouveau dataframe Arch où chaque colonne contient la moyenne et l'écart-type originaux
                                    
                                    #Les data frames Y et Arch sont assignés à l'objet "instance" en tant que membres "Y" et "Arch" pour stocker les résultats de la fonction: en devenant des membres de "instance", on pourra accéder facilement aux résultats de Y(données centrées et réduites) et au moyenne et écart type originaux (dans "Arch"). Y et Arch deviennent des attributs de l'objet "instance". 
                                    instance$Y <- Y
                                    instance$Arch <- Arch
                                    class(instance) <- "Scale" # la classe de l'objet "instance"est définie comme "Scale", utile pour identifier que cet objet est lié au centrage réduction des données.
                                    return(instance) #objet instance renvoyé en sortie de fonction, contient les données centrées et réduites et les moyennes et écart-type originaux. 
                                  }
                                  
                                  #centrage reduction de X (test set dataframe)
                                  #archive_moy_ect list contient la moyenne et ecart type de chaque colonne du training set Xtrain.On centre et réduit les données de Xtest avec moy et ect du Xtrain
                                  centering_red_pred <- function(X, Arch){
                                    instance <- list()
                                    cols <- colnames(X) #noms des colonnes de "X" sont extraits et stockées dans la variable "cols". 
                                    for(i in cols) { #boucle qui parcourt chaque colonne du dataframe X. 
                                      moy <- Arch[,i][1] #extrait la moyenne (1er élément) de la colonne 'i'à partir de archive.ect_mean et l'assigne à la variable "moy". 
                                      ect <- Arch[,i][2]
                                      X[i] <- (X[i] - moy)/ect # les données de la colonne 'i' de X sont centrées et réduites en utilisant la moyenne et ect de Xtrain.Ces nouvelles données remplacent les valeurs d'origine de X. 
                                    }
                                    instance$Xtest <- X #les données centreées et réduites stockées dans le dataframe 'X' sont assignées à l'objet 'instance' en tant que membre Xtest. 
                                    class(instance) <- "Xtestcr"
                                    return(instance)
                                  } 
                                  
                                  fit = function(X,y, prior = NULL) {
                                    if (!is.factor(y) && !is.character(y) && !is.logical(y)) {
                                      stop("y must be either a factor, character, or logical vector", call. = FALSE)
                                    }
                                    if (!is.factor(y)) {
                                      y <- factor(y) # si y n'est pas un factor, on le transforme en factor
                                    }
                                    levels <- levels(y) # on extrait les catégories(=classes) de y et on les mets dans la variable levels
                                    
                                    nlev <- nlevels(y) # on extrait le nombre de catégories de y, on a donc dans nlev le nombre de classes différentes dans y 
                                    vars <- colnames(X) # on extrait les noms des colonnes du data frame des variables explicatives et on les mets dans 'vars'
                                    class_x <- class(X)[1] #on extrait la "classe"= le type de données de l'objet 'X' et on stocke la première classe trouvée dans la variable 'classe_x'. Si X est un data frame, alors class_x prend la valeur 'data.frame'
                                    
                                    use_Matrix <- class_x %in% .matrix_classes # Cette ligne vérifie si la classe class_x est présente dans la liste .matrix_classes. La variable use_Matrix est définie comme TRUE si la classe de X correspond à l'une des classes de matrice spécifiées dans .matrix_classes, sinon elle est définie comme FALSE. En d'autres termes, cela détermine si X est de type matrice en fonction de sa classe.
                                    
                                    if (!is.matrix(X) && !use_Matrix) { #Cela signifie que si X n'est pas une matrice et que la classe de X n'indique pas qu'elle devrait être traitée comme une matrice, alors les opérations suivantes sont exécutées.
                                      warning("x was coerced to a matrix(x a été converti en matrice).", call. = FALSE)
                                      x <- as.matrix(X) #X est effectivement convertie en une matrice en utilisant la fonction as.matrix(), et le résultat est stocké dans la variable x.
                                      if (mode(X) != "numeric") { 
                                        #Cette condition vérifie si le mode des données dans X (c'est-à-dire le type de données) n'est pas "numeric".                                         Si le mode de X n'est pas numérique, cela signifie que X ne contient pas des données numériques, ce qui est                                           requis pour une matrice dans ce contexte.
                                        stop("x must be a matrix/dgCMatrix with integer columns.", call. = FALSE)
                                      }
                                    }
                                    
                                    if (use_Matrix) { #cette condition vérifie si "use_Matrix" est vraie, si oui, cela signifie que la classe de "X" devrait être traité comme une matrice spéciale. 
                                      if (!"Matrix" %in% rownames(utils::installed.packages())) {
                                        stop("Please install the \"Matrix\" package.", call. = FALSE)
                                      }
                                      #Cette condition vérifie si le package "Matrix" est installé. Pour ce faire, il vérifie si le nom "Matrix" est présent dans la liste des packages installés (récupérée à partir de rownames(utils::installed.packages())). Si le package n'est pas installé, le code génère une erreur avec le message "Please install the 'Matrix' package." en utilisant la fonction stop(). 
                                      if (class_x != "dgCMatrix") {
                                        stop("dgCMatrix class from the Matrix package is the only supported class.", call. = FALSE)
                                      }
                                    }
                          
                                    if (nlev < 2) {
                                      stop("y must contain at least two classes.", call. = FALSE)
                                    }
                                    
                                    if (is.null(vars)) { #On s'assure que les colonnes du data frame X ont des noms uniques
                                      stop("x must have unique column names.\n", call. = FALSE)
                                    }
                                    
                                    NAy <- anyNA(y) # si des valeurs manquantes sont présentes dans y, 'Nay' qui est une variable qui contient un vecteur logique, contiendra des valeurs True aux emplacements correspondants. 
                                    NAx <- anyNA(X)
                                    
                                  if (NAy) { # si Nay est vraie et donc qu'il y a au moins une valeur manquante dans le vecteur y:
                                      na_y_bool <- is.na(y) # Cette ligne crée un vecteur logique na_y_bool qui indique quels éléments de y sont des valeurs manquantes (TRUE) et lesquels sont des valeurs valides (FALSE). 
                                      len_na <- sum(na_y_bool) # compte le nombre de valeurs manquantes dans 'y' en additionnant les valeurs TRUE dans le vecteur na_y_bool. 
                                      warning(paste0("y contains ", len_na, " missing",
                                                     ifelse(len_na == 1, " value", " values"), ". ",
                                                     ifelse(len_na == 1, "It is", "They are"),
                                                     " not included (also the corresponding rows in x) ",
                                                     "into the estimation process."), call. = FALSE)
                                      y <- y[!na_y_bool] #on filtre le vecteur y pour ne conserver que les valeurs non manquantes, en le réassignant avec les valeurs non manquantes
                                      X <- X[!na_y_bool, ] #on filtre le dataframe X pour exclure les lignes correspondantes aux valeurs manquantes dans y, et s'assure que les lignes de X correspondent tjrs aux valeurs valides de y. 
                                    }
                                    
                                    if (NAx) { #Si au moins une valeur manquante dans X
                                      na_X <- is.na(X) * 1 #on crée une matrice na_X qui a la même dimension que X et qui contient des 1 pour chaque élément de X qui est un NA et des 0 pour les éléments valides.En multipliant le resultat is.na(X) par 1, on convertit les valeurs true en 1 et les valeurs false en 0. 
                                      len_nax <- sum(na_X) #on compte les valeurs manquantes dans X en additionnant tout les 1
                                      warning(paste0("x contains ", len_nax, " missing",
                                                     ifelse(len_nax == 1, " value", " values"), ". ",
                                                     "They are not included into the estimation process."),
                                              call. = FALSE)
                                    }
                                    
                                    y_counts <- stats::setNames(tabulate(y), levels) #y_counts représente le nbr d'occurences de chaque niveau (classe) dans le vecteur y. tabulate(y) est une fonction qui compte le nombre d'occurrences de chaque classe (ou niveau) dans le vecteur y. Elle renvoie un vecteur avec un élément par classe et la                                     fréquence (nombre d'occurrences) de chaque classe.
                                    #stats::setNames(...): Cette fonction permet d'attribuer des noms aux éléments du vecteur résultant de tabulate(y). Les noms attribués sont les niveaux (classes) de y, qui sont stockés dans le vecteur levels
                                    y_min <- y_counts <2 # y_min identifie les classes de y ayant moins de 2 occurences.le résultat est un vecteur de valeurs booléennes, où chaque élément est True si la classe a moins de 2 occurences, sinon False.
                                    
                                    if (any(y_min)) {
                                      stop("y variable has to contain at least two observation per class for the estimation process.", call. = FALSE)
                                    }
                                    
                                    ## Calcul des paramètres du modèle naive bayes gaussien (moyennne 'mu' et écarts-types 'sd' pour chaque classe)
                                    
                                    #calcul des probabilités a priori pour chaque classe:
                                    
                                    if (is.null(prior)) { # Si prior n'est pas spécifié: on vérifie que l'argument 'prior' est nul, cad s'il n'a pas été spécifié lors de l'appel à la fonction et on le calcul. 
                                      prior <- prop.table(y_counts)# prop.table divise chaque élément de y_counts (qui contient le nbr d'occurence de chaque classe du vecteur y) par la somme totale des éléments de y_counts. cad: calcul la proportion de chaque classe parmi l'ensemble des classes. 
                                    } else {
                                      if (length(prior) != nlev) #si 'prior' est spécifié, il est vérifié pour sa longueur et normalisé pour s'assurer qu'il représente une distribution de probabilité valide. On vérifie que les probabilités a priori correspondent aux classes présentes dans les données. 
                                        stop(paste0("gaussian_naive_bayes(): Vector with prior probabilities should have ",
                                                    nlev, " entries"))
                                      prior <- stats::setNames(prior / sum(prior), levels)# Si prior est correctement spécifié, ce code normalise les probabilités a priori. Il divise chaque probabilité a priori par la somme totale de toutes les probabilités a priori. Cela garantit que la somme de toutes les probabilités a priori est égale à 1. 
                                    }
                                    
                                    if (!NAx) { #Si la matrice X n'a pas de données manquantes, alors:
                                      if (use_Matrix) { #Si la matrice des données 'X' est de type 'Matrix", alors:
                                        #on itère sur les niveaux (classes) 'lev' et on calcul les paramètres mu et sd pour chaque classe:
                                        params <- do.call("rbind", lapply(levels, function(lev) {
                                          lev_subset <- X[y == lev, , drop = FALSE]
                                          mu <- Matrix::colMeans(lev_subset, na.rm = TRUE)
                                          sd <- sqrt((Matrix::colSums(lev_subset^2, na.rm = TRUE) - mu^2 * y_counts[lev]) / (y_counts[lev] - 1))
                                          rbind(mu, sd) }))
                                        #mu et sd pour chaque classe sont stockées dans une matrice 'params' où chaque ligne correspond à une classe. Les noms de ligne de params sont mis à jour pour correspondre aux niveaux (classes) dans le vecteur levels.
                                        mu <- params[rownames(params) == "mu", ]
                                        rownames(mu) <- levels
                                        sd <- params[rownames(params) == "sd", ]
                                        rownames(sd) <- levels
                                      } else { #si use_matrix est faux, calcul de mu et sd avec rowsum:
                                        mu <- rowsum(X, y, na.rm = TRUE) / y_counts
                                        sd <- sqrt((rowsum(X^2, y, na.rm = TRUE) - mu^2 * y_counts) / (y_counts - 1))
                                      }
                                      
                                    } else { #si Nax a des valeurs manquantes: 
                                        n <- if (use_Matrix) { #Le code calcule le nombre de valeurs manquantes par fonction de caractéristique (variable) pour chaque classe. Si le nombre de valeurs manquantes est inférieur à 2 pour une fonction de caractéristique, une alerte est générée:
                                        na_per_feature <- lapply(levels, function(lev) {
                                          Matrix::colSums(na_X[y == lev, , drop = FALSE], na.rm = TRUE)
                                        })
                                        n_feature_obs <- y_counts - do.call("rbind", na_per_feature)
                                        rownames(n_feature_obs) <- levels
                                        n_feature_obs
                                        } else {
                                        y_counts - rowsum.default(na_X, y)
                                        }
                                        if (any(n < 2))
                                        warning("gaussian_naive_bayes(): infinite variances (NaN) are present, ",
                                                "in each case due to less than two observations after removing missing values.", call. = FALSE)
                                      
                                        if (use_Matrix) {
                                          params <- do.call("rbind", lapply(levels, function(lev) {
                                            lev_subset <- x[y == lev, , drop = FALSE]
                                            mu <- Matrix::colMeans(lev_subset, na.rm = TRUE)
                                            nlev <- n[rownames(n) == lev]
                                            sd <- sqrt((Matrix::colSums(lev_subset^2, na.rm = TRUE) - mu^2 * nlev) / (nlev - 1))
                                            rbind(mu, sd) }))
                                          mu <- params[rownames(params) == "mu", ] ; rownames(mu) <- levels
                                          sd <- params[rownames(params) == "sd", ] ; rownames(sd) <- levels
                                        } else {
                                          mu <- rowsum(x, y, na.rm = TRUE) / n
                                          sd <- sqrt((rowsum(x^2, y, na.rm = TRUE) - mu^2 * n) / (n - 1))
                                        }
                                    }
                                    
                                    private$data <- list(x = X, y = y)
                                    private$levels <- levels
                                    private$params <- list(mu = mu, sd = sd)
                                    private$prior <- prior
                                    private$call <- match.call()
                                  }, #fin de la fonction "fit"
                                    
                                  ## Méthode de prédiction ##
                                  
                                  predict = function(newdata, type = "class", threshold = 0.001, eps = 0) {
                                    #on vérifie que les données d'entrée 'newdata' ont été fournies:
                                    if (is.null(newdata))
                                      stop("predict.gaussian_naive_bayes(): newdata is required.", call. = FALSE)
                                    #on vérifie que 'newdata' est une matrice numérique:
                                    class_x <- class(newdata)[1]
                                    use_Matrix <- class_x == "dgCMatrix"
                                    
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
                                    
                                    
                                    #Les caractéristiques de newdata sont comparées à celles du modèle, et seules les caractéristiques correspondantes sont utilisées pour les prédictions. Les caractéristiques non correspondantes sont ignorées:
                                    
                                    if (n_features == 0) {
                                      warning(paste0("predict.gaussian_naive_bayes(): no feature in newdata corresponds to ",
                                                     "features defined in the object. Classification is based on prior probabilities."), call. = FALSE)
                                      if (type == "class") {
                                        return(factor(rep(lev[which.max(prior)], n_obs), levels = lev))
                                      } else {
                                        return(matrix(prior, ncol = n_lev, nrow = n_obs, byrow = TRUE, dimnames = list(NULL, lev)))
                                      }
                                    }
                                    
                                    if (n_features < n_tables) {
                                      warning(paste0("predict.gaussian_naive_bayes(): only ", n_features, " feature(s) in newdata could be matched ",
                                                     "with ", n_tables, " feature(s) defined in the object."), call. = FALSE)
                                    }
                                    if (n_features_newdata > n_features) {
                                      warning(paste0("predict.gaussian_naive_bayes(): newdata contains feature(s) that could not be matched ",
                                                     "with (", n_features, ") feature(s) defined in the object. Only matching features are used for calculation."), call. = FALSE)
                                      newdata <- newdata[, features, drop = FALSE]
                                    }
                                    
                                    # Si des données manquantes sont présentes dans newdata, elles sont identifiées. Les valeurs manquantes sont remplies de zéros dans le calcul des probabilités, et un avertissement est affiché pour informer de la présence de données manquantes:
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
                                    
                                    #Calcul des probabilités :Les probabilités pour chaque classe sont calculées à partir des caractéristiques de newdata en utilisant la densité gaussienne.Les probabilités sont calculées pour chaque classe et stockées dans la matrice post:
                
                                    post <- matrix(nrow = n_obs, ncol = n_lev)
                                    for (ith_class in seq_along(lev)) {
                                      ith_class_sd <- sd[ith_class, ]
                                      ith_post <- -0.5 * log(2 * pi * ith_class_sd^2) - 0.5 * ((newdata - mu[ith_class, ]) / ith_class_sd)^2
                                      if (NAx) ith_post[ind_na] <- 0
                                      ith_post[ith_post <= eps] <- threshold
                                      post[, ith_class] <- if (use_Matrix) Matrix::colSums(ith_post) + log(prior[ith_class]) else colSums(ith_post) + log(prior[ith_class])
                                    }
                                    
                                    #Classification ou probabilités :Selon le paramètre type, la fonction peut renvoyer les classes prédites ou les probabilités associées.Si type est "class", la classe prédite est renvoyée. Si une seule observation est fournie, la classe la plus probable est renvoyée.Si type est "prob", les probabilités sont renvoyées. Pour une seule observation, les probabilités sont normalisées pour former une distribution de probabilité sur les classes:
                                    
                                    if (type == "class") {
                                      if (n_obs == 1) {
                                        return(factor(lev[which.max(post)], levels = lev))
                                      } else {
                                        return(factor(lev[max.col(post, "first")], levels = lev))
                                      }
                                    }
                                    else {
                                      if (n_obs == 1) {
                                        post <- t(as.matrix(apply(post, 2, function(x) { 1 / sum(exp(post - x)) })))
                                        colnames(post) <- lev
                                        return(post)
                                      }
                                      else {
                                        colnames(post) <- lev
                                        return(apply(post, 2, function(x) { 1 / if (use_Matrix) Matrix::rowSums(exp(post - x)) else rowSums(exp(post - x)) }))
                                      }
                                    }
                                    
                                 ## Méthode predict_proba ##
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                      
                                    }, #fin de la fonction "predict.proba"
                            
                                  
                                  predict_proba = function(X) {
                                      # Code pour renvoyer les probabilités
                                      # ...
                                    }
                                
                               
                               private = list(
                                 # Membres privés
                                 data = NULL,
                                 # ...
                               )
)
                                  