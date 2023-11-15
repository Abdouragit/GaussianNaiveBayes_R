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