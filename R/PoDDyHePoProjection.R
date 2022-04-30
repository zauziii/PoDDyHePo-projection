 #' Description of PoDDyHePoProjection()
#' 
#' @title Projection Function Developed for PoDDy-HePo Project
#'
#' @description PoDDyHePoProjection is the main body for projection. It projects projects individual-level values of the variables using multiple imputation.
#' 
#' @details It allows people to set up a variable whose proportion of each category needs to be calculated and returns a \code{\link[=mids-class]{mids}} object.
#' 
#' @param data A well-formatted data set ready for projection.
#' @param m Number of multiple imputations. The default is \code{m=5}.
#' @param maxit A scalar giving the number of iterations. By default, `maxit = 10`.
#' @param printFlag If `TRUE`, `mice` will print history on console. Use `print=FALSE` for silent computation.
#' @param ... Formulas. Formulas.
#'
#' @return Returns an S3 object of class \code{\link[=mids-class]{mids}}
#'        (multiply imputed data set)
#'        
#' @export
#' 
#' @import mice
#' @importFrom fastDummies dummy_cols
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
