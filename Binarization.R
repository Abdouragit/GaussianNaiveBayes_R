
# Binarization method

binarize <- function(column) {
  
  if(is.numeric(column)) {
    return(column)
  }else if (is.factor(column) || is.character(column)) {
    unique_values <- unique(column) #different modalities of the binarized variable
    binary_columns <- lapply(unique_values, function(value) ifelse(column == value, 1, 0))
    names(binary_columns) <- paste0(names(column), "_", unique_values)
    return(as.data.frame(binary_columns))
  } else {
    stop("Only character, factor or numerical columns must be entered.")
  }
}


#in the fit function

self$x <- lapply(self$x, binarize)
self$x = cbind(as.data.frame(self$x))
