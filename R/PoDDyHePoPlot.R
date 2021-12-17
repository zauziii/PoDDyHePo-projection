#' Description of PoDDyHePoPlot()
#' 
#' @title Plot Function Developed for PoDDy-Hepo Project
#'
#' @description It plots the prevalences against survey year.
#'
#' @details Enables plotting even there is a variable whose porprotion of each category has been calculated.
#'
#' @param data A data frame from PoDDyHePoPool.
#' @param year The maximum year in the obseved data.
#' @param title The tile of the plot. It changes due to we will have different reponse variable.
#' @param sepvarlbl Lable of the variable whose porprotion of each category has been calculated. The default is \code{NULL}. Accoring to dummy_cols, the separated variables will be in the name for example obe_0, obe_1, obe_2,
#' if we taking obe for example. To label, sepvarlbl = c("Normal Weight", "Over Weight", "Obesity"). As the levels are sorted, so please make sure that the labels are in right order. 
#'
#'
#' @return Returns a figure
#' @export
#' 
#' @import ggplot2


PoDDyHePoPlot <- function(data, year, title, sepvarlbl = NULL){
  
  # 
  if(nrow(data) != length(unique(data$Year)) * 2 ){
    
    # Levels for seperated variable
    levels <- sort(unique(data$Group))
    
    # Label seperated variable
    data$Group <- factor(data$Group, 
                         labels = sepvarlbl,
                         levels = levels)
    
    # Label Sex
    data$Sex <- factor(data$Sex,
                       levels = c(1, 2),
                       labels = c("Men", "Women"))
    
    # Plot
    ggplot(data = data[data$Year <= year, ], aes(x = .data$Year, y = 100 * .data$EST, color = .data$Group) ) +
      geom_point(shape = 16, size = 2, position = position_dodge(width = 2.5)) +
      geom_point(data = data[data$Year > year, ], shape = 1, size = 1.5, position = position_dodge(width = 2.5)) +
      geom_line(size = 0.7, position = position_dodge(width = 2.5)) +
      geom_line(data = data[data$Year >= year, ], linetype = "dashed", size = 0.7, position = position_dodge(width = 2.5)) +
      geom_errorbar(aes(ymin = 100 * .data$CI_LOWER,
                        ymax = 100 * .data$CI_UPPER),
                    width = 3,
                    position = position_dodge(width = 2.5)) +
      geom_errorbar(data = data[data$Year > year, ],
                    aes(ymin = 100 * .data$CI_LOWER,
                        ymax = 100 * .data$CI_UPPER),
                    width = 3,
                    position = position_dodge(width = 2.5)) +
      scale_color_manual(name = " ",
                         labels = sepvarlbl,
                         values = c("grey25", "red", "blue")) + 
      scale_y_continuous(name = "%",
                         limits = c(min(100 * data$CI_LOWER, 0), ceiling(max(100 * data$CI_UPPER) + 5)),
                         breaks = seq(0, ceiling(max(100 * data$CI_UPPER)) + 5, 10)) +
      scale_x_continuous(breaks = unique(data$Year)) +
      ggtitle(title) +
      facet_wrap( ~ Sex) +
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
            panel.spacing = unit(0.35, "lines"))
  } else{
    
    # Plot (not seperated variable)
    ggplot(data = data[data$Year <= year, ], aes(x = .data$Year, y = 100 * .data$EST, group = .data$Sex, color = .data$Sex)) +
      geom_line(size = 1, position = position_dodge(width = 1.5)) +
      geom_line(data = data[data$Year >= year, ], linetype = "dashed", size = 1, position = position_dodge(width = 1.5)) +
      geom_point(shape = 16, size = 2.5, show.legend = T, position = position_dodge(width = 1.5)) +
      geom_point(data = data[data$Year > year, ], shape = 21, size = 2.5, show.legend = T, position = position_dodge(width = 1.5)) +
      geom_errorbar(aes(ymin = 100 * .data$CI_LOWER,
                        ymax = 100 * .data$CI_UPPER),
                    width = 1.5,
                    position = position_dodge(width = 1.5)) +
      geom_errorbar(data = data[data$Year > year, ],
                    aes(ymin = 100 * .data$CI_LOWER,
                        ymax = 100 * .data$CI_UPPER),
                    width = 1.5,
                    position = position_dodge(width = 1.5)) +
      scale_color_manual(name = "",
                         labels = c("Men", "Women"),
                         values = c("blue", "red")) +
      scale_y_continuous(name = "%",
                         limits = c(min(100 * data$CI_LOWER, 0), ceiling(max(100 * data$CI_UPPER) + 5)),
                         breaks = seq(0, ceiling(max(100 * data$CI_UPPER)) + 5, 10)) +
      scale_x_continuous(breaks = unique(data$Year)) +
      theme_bw() +
      ggtitle(title) +
      theme(plot.title = element_text(hjust = 0.5 ,margin = margin(5, 0, 15, 0), size = 28),
            axis.title.x = element_text(margin = margin(5, 0, 5, 0), size = 22),
            axis.text.x = element_text(size = 18, angle = 45, vjust = 0.5),
            axis.title.y = element_text(margin = margin(0, 5, 0, 10), size = 22),
            axis.text.y = element_text(size = 16),
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 20),
            legend.margin=margin(0, 0, 0, 0)) +
      theme(aspect.ratio = 2/3)
  }
  
}
