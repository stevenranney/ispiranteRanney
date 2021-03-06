\name{PlotScoreByCallVolume}
\alias{PlotScoreByCallVolume}
\title{Create a plot of the score by call volume}
\description{This function plots the satisfaction score and produces a plot 
of those values as a function of call volume. }
\usage{PlotScoreByCallVolume(dF, ...)}
\arguments{
  \item{dF}{The dataFrame for which to convert \code{satisfaction_score} into a
number.}
  \item{\dots}{Arguments to be passed to other functions.  Specifically, this can take
the form of identifying which \code{date} value is of interest.  For example
the sample datasets provided all had either at least \code{assigned_at} 
or \code{created_at} values.  The \code{\link{AssignDateAndDay}} can used
either of these values as the date of interest. }
}
\details{This function calls the \code{\link{ConvertSatisfactionScoreToNumber}} 
function to assign the \code{day} values used.  

Plots are generated by the function \code{\link{ggplot}}

This function uses a linear model to evaluate the relationship between
satisfaction score and call volume (i.e., calls per day).  If the red line 
(the result of the \code{\link{lm}} in the produced plot trends down, then 
resolution time has a negative affect on satisfaction score.  If the red 
line is parallel or very near to parallel, then there is no relationship 
between satisfaction score and resolution time.

In most cases, there are very few or no satisfaction scores.  As a 
result, these data are sparse.}


\author{Steven H. Ranney
Contact: \email{Steven.Ranney@gmail.com}
Steven Ranney}





