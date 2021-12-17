 #' Description of PoDDyHePoProjection()
#' 
#' @title Projection Function Developed for PoDDy-HePo Project
#'
#' @description PoDDyHePoProjection is the main body for projection. It projects projects individual-level values of the variables using multiple imputation.
#' 
#' @details It allows people to set up a variable whose proportion of each category needs to be calculated and returns a \code{\link[=mids-class]{mids}} obeject.
#' 
#' @param data A well-formatted data set ready for projection.
#' @param m Number of multiple imputations. The default is \code{m=5}
#' @param sep_col A variable (usually has 3 levels or more) to be seperated. 
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

PoDDyHePoProjection <- function(data, m = 5, sep_col = NULL, ...){   # ...
  
  # Imputation
  imp <- mice(data,
              m,
              maxit = 10,
              formulas = name.formulas(list(...)),
              printFlag = F
  )
  
  # If sep_col is NULL, the program do not calculate the proportion of each level.
  if(!is.null(sep_col)){

    # Long format
    imp.complete <- complete(imp, action = "long", include = T) 
    
    # Calculated the proportion for each level (Create dummies) - if necesssary
    imp.complete <- imp.complete %>% 
      fastDummies::dummy_cols(select_columns = sep_col, ignore_na = T)
    
    # Transform back to mids
    imp <- as.mids(imp.complete)
  }
  
  return(imp)
}
