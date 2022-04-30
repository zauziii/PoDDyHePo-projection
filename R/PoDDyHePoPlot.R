#' Description of PoDDyHePoPlot()
#' 
#' @title Plot Function Developed for PoDDy-Hepo Project
#'
#' @description It plots the prevalences against survey year.
#'
#' @details Enables plotting even there is a variable whose proportion of each category has been calculated.
#'
#' @param data A data frame from PoDDyHePoPool.
#' @param year The maximum year in the observed data.
#' @param grplabels The labels of grouping variable. By default, grplabels = c("Men", "Women").
#' @param title The tile of the plot. It changes due to we will have different response variable.
#' @param y_min minimum value of y-axis(in percentage). For example, if 10 percent is the minimum value of y-axis, then type let y_min = 10.
#' @param y_max maximum value of y-axis(in percentage). Let y_max = 75, if 75 percent is the maximum value of y-axis.
#' @param sepvarlbl Label of the variable whose proportion of each category has been calculated. The default is \code{NULL}. According to dummy_cols, the separated variables will be in the name for example obe_0, obe_1, obe_2, if we taking obe for example. To label, sepvarlbl = c("Normal Weight", "Over Weight", "Obesity"). As the levels are sorted, so please make sure that the labels are in right order. 
#'
#'
#' @return Returns a figure
#' @export
#' 
#' @import ggplot2


PoDDyHePoPlot <- function(data, year, grplabels = c("Men", "Women"), title, y_min, y_max, sepvarlbl = NULL){
  
  # Rename variables
  if(ncol(data) != 5){
    colnames(data) <- c("Year", "grpVar", "EST", "CI_LOWER", "CI_UPPER", "Group")
  } else{
    colnames(data) <- c("Year", "grpVar", "EST", "CI_LOWER", "CI_UPPER")
  }
  
  if(is.null(grplabels)){
    grplabels <- sort(unique(data$grpVar))
  }
  
  if(nrow(data) != (length(unique(data$Year)) * length(unique(data$grpVar)))){
    
    # Levels for seperated variable
    levels <- sort(unique(data$Group))
    
    # Label seperated variable
    if(is.null(sepvarlbl)){
      sepvarlbl <- levels
    }
    
    data$Group <- factor(data$Group, 
                         labels = sepvarlbl,
                         levels = levels)
    
    # Label grouping variable
    data$grpVar <- factor(data$grpVar,
                          levels = sort(unique(data$grpVar)),
                          labels = grplabels)
    
    # Plot
    ggplot(data = data[data$Year <= year, ], aes(x = .data$Year, y = 100 * .data$EST, color = .data$grpVar, group = .data$Group)) +
      geom_point(aes(fill = .data$grpVar, shape = .data$Group), size = 2.5, position = position_dodge(width = 2.5)) +
      geom_point(aes(shape = .data$Group), data = data[data$Year > year, ], size = 2.5, position = position_dodge(width = 2.5)) +
      geom_line(size = 0.65, position = position_dodge(width = 2.5)) +
      geom_line(data = data[data$Year >= year, ], linetype = "dashed", size = 0.65, position = position_dodge(width = 2.5)) +
      geom_errorbar(aes(ymin = 100 * .data$CI_LOWER,
                        ymax = 100 * .data$CI_UPPER),
                    width = 3,
                    position = position_dodge(width = 2.5)) +
      geom_errorbar(data = data[data$Year > year, ],
                    aes(ymin = 100 * .data$CI_LOWER,
                        ymax = 100 * .data$CI_UPPER),
                    width = 3,
                    position = position_dodge(width = 2.5)) +
      scale_y_continuous(name = "%",
                         limits = c(y_min, y_max),
                         breaks = c(seq(0, floor(y_max/10)*10, 10), y_max)) +
      scale_x_continuous(breaks = unique(data$Year)) +
      scale_shape_manual(name = " ",
                         labels = sepvarlbl,
                         values = c(21, 24, 22, 23, 25)) + 
      scale_fill_manual(guide = "none",
                        values = c("blue", "red", "black", "#61D04F", "#28E2E5", "#CD0BBC", "#F5C710", "gray62")) +
      ggtitle(title) +
      facet_wrap( ~ Sex) +
      scale_color_manual(name = " ",
                         labels = c("Men", "Women"),
                         values = c("blue", "red", "black", "#61D04F", "#28E2E5", "#CD0BBC", "#F5C710", "gray62"),
                         guide = "none") + 
      theme_bw() + 
      theme(plot.title = element_text(hjust = 0.5 ,margin = margin(5, 0, 15, 0), size = 28),
            axis.title.x = element_text(margin = margin(5, 0, 5, 0), size = 20),
            axis.text.x = element_text(size = 14, angle = 45, vjust = 0.5),
            axis.title.y = element_text(margin = margin(0, 5, 0, 10), size = 20),
            axis.text.y = element_text(size = 18),
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 20),
            legend.margin=margin(0, 0, 0, 0),
            strip.text.x = element_text(size = 18),
            strip.background = element_rect(fill = "gray95", color = "black"),
            panel.border = element_rect(color = "black"),
            panel.spacing = unit(0.35, "lines"),
            panel.grid.minor = element_blank())
  } else{
    
    # Plot (not seperated variable)
    ggplot(data = data[data$Year <= year, ], aes(x = .data$Year, y = 100 * .data$EST, color = .data$grpVar)) +
      geom_line(size = 0.65, position = position_dodge(width = 1.5)) +
      geom_line(data = data[data$Year >= year, ], linetype = "dashed", size = 0.65, position = position_dodge(width = 1.5)) +
      geom_point(aes(fill = .data$grpVar, shape = .data$grpVar), size = 3, position = position_dodge(width = 1.5)) +
      geom_point(aes(shape = .data$grpVar), data = data[data$Year > year, ], size = 3, position = position_dodge(width = 1.5)) +
      geom_errorbar(aes(ymin = 100 * .data$CI_LOWER,
                        ymax = 100 * .data$CI_UPPER),
                    width = 1.5,
                    position = position_dodge(width = 1.5)) +
      geom_errorbar(data = data[data$Year > year, ],
                    aes(ymin = 100 * .data$CI_LOWER,
                        ymax = 100 * .data$CI_UPPER),
                    width = 1.5,
                    position = position_dodge(width = 1.5)) +
      scale_y_continuous(name = "%",
                         limits = c(y_min, y_max),
                         breaks = c(seq(0, floor(y_max/10)*10, 10), y_max)) +
      scale_x_continuous(breaks = unique(data$Year)) +
      scale_color_manual(name = " ",
                         labels = grplabels,
                         values = c("blue", "red", "black", "#61D04F", "#28E2E5", "#CD0BBC", "#F5C710", "gray62")) + 
      scale_shape_manual(name = " ",
                         labels = grplabels,
                         values = c(21, 24, 22, 23, 25)) + 
      scale_fill_manual(guide = "none",
                        values = c("blue", "red", "black", "#61D04F", "#28E2E5", "#CD0BBC", "#F5C710", "gray62")) +
      theme_bw() +
      ggtitle(title) +
      theme(plot.title = element_text(hjust = 0.5 ,margin = margin(5, 0, 15, 0), size = 28),
            axis.title.x = element_text(margin = margin(5, 0, 5, 0), size = 22),
            axis.text.x = element_text(size = 16, angle = 45, vjust = 0.5),
            axis.title.y = element_text(margin = margin(0, 5, 0, 10), size = 22),
            axis.text.y = element_text(size = 18),
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 24),
            legend.margin = margin(0, 0, 0, 0),
            panel.grid.minor = element_blank()) +
      theme(aspect.ratio = 2/3)
  }
  
}
