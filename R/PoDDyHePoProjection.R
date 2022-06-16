#' Description of PoDDyHePoProjection()
#' 
#' @title Projection Function Developed for PoDDy-HePo Project
#'
#' @description PoDDyHePoProjection() is the main body for projection. It projects individual-level values of the variables using multiple imputation.
#' 
#' @details It allows user to set up basic settings like number of multiple imputations(\code{m}), number of iterations(\code{maxit}) and whether to show the history(\code{printFlag}). It returns a \code{\link[=mids-class]{mids}} object if and only if when every variable with missingess has given a formula.
#' 
#' @param data A well-formatted data set ready for projection.
#' @param m Number of multiple imputations. The default is \code{m=5}.
#' @param maxit A scalar giving the number of iterations. By default, `maxit = 10`.
#' @param printFlag If `TRUE`, `mice` will print history on console. Use `print=FALSE` for silent computation.
#' @param ... Formulas.
#'
#' @return Returns an S3 object of class \code{\link[=mids-class]{mids}}
#'        (multiply imputed data set)
#' 
#' @import mice
#' @importFrom magrittr %>% 

PoDDyHePoProjection <- function(data, m = 5, maxit = 10, printFlag = F,...){   # ...
  
  # Imputation
  imp <- mice(data,
              m,
              maxit = maxit,
              formulas = name.formulas(list(...)),
              printFlag = printFlag
  )
  
}
