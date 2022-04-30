#' Description of PoDDyHePoModelSelection()
#' 
#' @title Model Selection Function Developed for PoDDy-HePo Project (BIC based)
#'
#' @description PoDDyHePoModelSelection returns a list of 4 elements: 
#'              Models (Records all selected models of each imputed data set), 
#'              Mean BIC (Records Mean BIC of each round of imputation),
#'              Selected Models (Records a selected model/selected models over imputation of cases)
#'              Suggested Model (Records a suggested model with smaller BIC value)
#' 
#' @details When NsVar = NULL, there is only one BIC value in Mean BIC and one model in Selected Models, the model in Selected Models is the same as in Suggested Model.
#'          df or knots should be denied.
#'          When both knots and b.knots are specified, df will be ignored.
#'
#' @param imp An object of class \code{mids} (produced by \code{mice()}.
#' @param DV Abbreviation of Dependent Variable. 
#' @param NsVar A specific variable to which would apply nature splines. The default is \code{NULL}.
#' @param df Degrees of freedom. The default is \code{df=NULL}.  
#' @param knots Breakpoints that define the spline.
#' @param b.knots Boundary points at which to impose the natural boundary conditions and anchor the B-spline basis (default the range of the data).
#' @param f.var Fixed variables. Variables that would like to fix it the model (will not be removed by stepwise process).
#'
#' @return Returns a list with 4 elements: Models, Mean BIC, Selected Models and Suggested Model.
#' 
#' @importFrom magrittr %>% 
#' @importFrom splines ns
#' @importFrom stringi stri_replace_all_regex
#' 
#' @export

PoDDyHePoModelSelection <- function(imp, DV, NsVar = NULL, df = NULL, knots = NULL, b.knots = NULL, f.var = NULL){
  
  # If NsVar is not null, create replaced variables for main effects and interaction term
  if(!is.null(NsVar)){
    Ns <- list()
    replace <- data.frame(matrix(NA, max(length(df)+1, 2), length(NsVar)))
    fix.var <- data.frame(matrix(NA, max(length(df)+1, 2), length(f.var)))
    Ns[[1]] <- names(imp$data)
    if(is.null(f.var)){
      fix.var[1, ] <- list(NULL)
    } else{
      fix.var[1, ] <- f.var
    }
    
    # knots= specified
    if(!is.null(knots)){
      if(is.null(b.knots)){
        if(!is.null(knots)){
          replace[max(length(df)+1, 2), ] <- paste0("splines::ns(", NsVar, 
                                                    ",knots=", "c(", knots, ")", ")")
        }
      } else{
        replace[max(length(df)+1, 2), ] <- paste0("splines::ns(", NsVar, 
                                                  ",knots=", paste0("c(", knots ,")"), 
                                                  ",Boundary.knots=", b.knots, ")")
        if(!is.null(df)){
          cat("NOTE: Both knots= and b.knots= specified, df ignored")
        } 
      }
    }
    
    
    # df= specified
    if(is.null(knots) & !is.null(df)){
      for (i in 2:(length(df)+1)) {
        replace[i, ] <- paste0("splines::ns(", NsVar, ",df=", df[i-1], ")")
      }
    }
    
    # If df= and knots= not specified
    if(is.null(df) && is.null(knots)){
      stop("NOTE: Both df= and knots= are missing, input as least one of them")
    }
    
    for (i in 2:max((length(df)+1),2)){
      Ns[[i]] <- names(imp$data)
      for (j in 1:length(NsVar)) {
        Ns[[i]][Ns[[i]] == NsVar[j]] <- replace[i, j]
      }
      
      if(is.null(f.var)){
        fix.var[i, ] <- NA
      } else{
        fixvars <- Ns[[i]][which(f.var %in% names(imp$data))]
        # If NsVar is in the interaction term, replace it and attach to f.var2,
        
        if(grepl(paste0(".*", NsVar, collapse = "|"), f.var[which(!f.var %in% names(imp$data))])){
          fix.var[i, ] <- append(fixvars, stringi::stri_replace_all_regex(f.var[which(!f.var %in% names(imp$data))], 
                                                                          NsVar, 
                                                                          replace[2, ], 
                                                                          vectorize = F))
        } else{
          fix.var[i, ] <- append(fixvars, f.var[which(!f.var %in% names(imp$data))])
        } 
      }
      
    }
  } else{
    # If NsVar=NULL, df=, knots= and b.knots= ignored
    cat("NOTE: NsVar= not specified, ignored df=, knots= and b.knots=")
    
    Ns[[1]] <- names(imp$data)
    if(is.null(f.var)){
      fix.var[1, ] <- list(NULL) 
    } else{
      fix.var[1, ] <- f.var
    }
  }
  
  # Model Selection for each imputed data set
  model_selection <- function(imp, x_vars, y_var, fixed_vars) {
    
    if (is.null(fixed_vars)) {
      with(imp, {f1 <-  glm(as.formula(sprintf("%s ~ %s", y_var, "1")), family = binomial(link = "logit"))
      f2 <-  step(f1,
                  scope = list(upper = as.formula(sprintf("%s ~ %s", y_var, paste(c("("), paste(x_vars, collapse = "+"), c(")^2")))),
                               lower = as.formula(sprintf("%s ~ %s", y_var, "1"))),
                  direction = c("both"),
                  k = log(nrow(imp$data)),
                  trace = 0)
      }
      )
    } else{
      with(imp, {f1 <-  glm(as.formula(sprintf("%s ~ %s", y_var, paste(fixed_vars, collapse = " + "))), family = binomial(link = "logit"))
      f2 <-  step(f1,
                  scope = list(upper = as.formula(sprintf("%s ~ %s", y_var, paste(c("("), paste(x_vars, collapse = " + "), c(")^2")))),
                               lower = as.formula(sprintf("%s ~ %s", y_var, paste(fixed_vars, collapse = " + ")))),
                  direction = c("both"),
                  k = log(nrow(imp$data)),
                  trace = 0)
      }
      )
    }
  }
  
  # Model Fitting
  fit <- BIC <- selectedModels <- list()
  
  if(is.null(NsVar)){
    m <- 1
  } else{
    m <- (max(length(df)+1, 2))
  }
  
  for (i in 1:m) {
    ## Fit model for each impute data set
    fit[[i]] <- model_selection(imp, Ns[[i]][Ns[[i]]!=DV], DV, fix.var[i, ])
    
    ## Compute Mean BIC
    BIC[[i]] <- fit[[i]]$analyses %>% sapply(BIC) %>% mean()
    
    ## Index of the minimun Mean BIC
    idx <- which.min(BIC)
    
    ## Selected moldes from each imputed data set
    selectedModels[[i]] <- fit[[i]]$analyses[[fit[[i]] $analyses %>% sapply(BIC) %>% which.min()]]$formula
    
    ## Suggested model
    suggestedModel <- fit[[idx]]$analyses[[fit[[idx]]$analyses %>% sapply(BIC) %>% which.min()]]$formula
  }
  
  
  # Combination
  info <- list(fit,
               BIC,
               selectedModels,
               suggestedModel)
  
  # Name the list elements
  names(info) <- c("Models", "Mean BIC", "Selected Models", "Suggested Model")
  
  # Name sublist
  if(!is.null(NsVar)){
    
    ## knots= specified
    if(!is.null(knots)){
      if(is.null(b.knots)){
        if(!is.null(knots)){
          names(info$`Mean BIC`) <- names(info$`Selected Models`) <- c("Non-splines", 
                                                                       paste0("ns(", NsVar, 
                                                                              ",knots=", "c(", knots, ")", ")"))
        }
      } else{
        names(info$`Mean BIC`) <- names(info$`Selected Models`) <- c("Non-splines", paste0("ns(", NsVar, 
                                                                                           ",knots=", paste0("c(", knots ,")"), 
                                                                                           ",Boundary.knots=", b.knots, ")", collapse = " & "))
      }
    }
    
    # df= specified
    if(is.null(knots) & !is.null(df)){
      for (i in 2:(length(df)+1)) {
        names(info$`Mean BIC`) <- names(info$`Selected Models`) <- c("Non-splines", paste0("DF=",df))
      }
    }
    
  } else{
    names(info$`Mean BIC`) <- names(info$`Selected Models`) <- c("Non-splines")
  }
  
  return(info)
}
