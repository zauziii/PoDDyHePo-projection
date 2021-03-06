% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PoDDyHePoPlot.R
\name{PoDDyHePoPlot}
\alias{PoDDyHePoPlot}
\title{Plot Function Developed for PoDDy-Hepo Project}
\usage{
PoDDyHePoPlot(
  data,
  year,
  grplabels = NULL,
  title,
  y_min,
  y_max,
  sepvarlbl = NULL
)
}
\arguments{
\item{data}{A data frame from `PoDDyHePoPool()`.}

\item{year}{The maximum year in the observed data.}

\item{grplabels}{The labels of grouping variable. By default, grplabels = NULL. In our case, grplabels = c("Men", "Women").}

\item{title}{Title for the plot. It changes due to different response variable.}

\item{y_min}{minimum value of y-axis(in percentage). For example, if 10 percent is the minimum value of y-axis, then type let y_min = 10.}

\item{y_max}{maximum value of y-axis(in percentage). Let y_max = 75, if 75 percent is the maximum value of y-axis.}

\item{sepvarlbl}{Labels of the variable whose proportion of each category has been calculated. The default is \code{NULL}. If it is \code{NULL}, the name of variable will then be like obe_0, obe_1, obe_2 take our case for example. If it is labelled, as the levels are sorted, please make sure that the labels are in right order.}
}
\value{
Returns a figure
}
\description{
It plots the prevalences against survey year.
}
\details{
Description of PoDDyHePoPlot()

Enables plotting even there is a variable whose proportion of each category has been calculated.
}
