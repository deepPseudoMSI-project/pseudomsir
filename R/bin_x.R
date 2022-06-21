###bin function---------------------------------------------------------------------------
#' @title bin_x
#' @description bin_x
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param x x
#' @param x_min x_min
#' @param x_max x_max
#' @param pixel pixel
#' @return a new vector
#' @export

bin_x <-
  function(x,
           x_min = 60,
           x_max = 1000,
           pixel = 224) {
    if (min(x) < x_min |
        max(x) > x_max) {
      stop("The x_min must smaller than the min of x and the x_max must be bigger than the max of x.\n")
    }
    
    index <- seq(x_min, x_max, length.out = pixel + 1)
    
    index1 <- index[-length(index)]
    index2 <- index[-1]
    
    index <- data.frame(index1,
                        index2,
                        cell = 1:length(index1),
                        stringsAsFactors = FALSE)
    
    cell <-
      sapply(x, function(x) {
        idx <- which((x - index$index1) >= 0 & (x - index$index2) < 0)[1]
        return(index$cell[idx])
      })
    
    return(cell)
  }




###bin function---------------------------------------------------------------------------
#' @title bin_x2
#' @description bin_x2
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param x x
#' @param x_min x_min
#' @param x_max x_max
#' @param pixel pixel
#' @return a new vector
#' @export

bin_x2 <-
  function(x,
           x_min = 60,
           x_max = 1000,
           pixel = 224) {
    if (min(x) < x_min |
        max(x) > x_max) {
      stop("The x_min must smaller than the min of x and the x_max must be bigger than the max of x.\n")
    }
    
    index <- seq(x_min, x_max, length.out = pixel + 1)
    
    index1 <- index[-length(index)]
    index2 <- index[-1]
    
    index <- data.frame(index1,
                        index2,
                        cell = 1:length(index1),
                        stringsAsFactors = FALSE)
    
    cell <- vector(mode = "numeric", length = length(x))
    
    for (i in seq_len(nrow(index))) {
      cell[which(x >= index$index1[i] & x <= index$index2[i])] <-
        index$cell[i]
    }
    
    if (any(cell == 0)) {
      stop("no index for cell.")
    }
    
    return(cell)
  }
