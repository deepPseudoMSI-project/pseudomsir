####-------------------------------------------------------------------
###bin function---------------------------------------------------------------------------
#' @title get_mz_shift
#' @description get_mz_shift
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param mz mz
#' @param mean mean
#' @param sd sd
#' @return a new vector
#' @export

get_mz_shift <-
  function(mz = c(87.04406, 90.05499, 94.06512, 400.1234),
           mean = 1.12361,
           sd = 4.076444) {
    error <- rnorm(n = 10000, mean = mean, sd = sd)
    
    temp_error <- sample(error, length(mz), replace = TRUE)
    mz <-
      mz - (ifelse(mz < 400, 400, mz) * temp_error) / 10 ^ 6
    
    # mz <- purrr::map(
    #   mz,
    #   .f = function(x) {
    #     temp_error <- sample(error, 1)
    #     y <- x - (ifelse(x < 400, 400, x) * temp_error) / 10 ^ 6
    #     y
    #   }
    # )
    return(mz)
  }
